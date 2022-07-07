package app.softwork.kobol

import com.intellij.psi.*
import com.intellij.psi.tree.*
import com.intellij.psi.util.*

fun CobolFile.toTree(): CobolFIRTree {
    var fileComments: List<String>? = null
    var id: CobolFIRTree.ID? = null
    var env: CobolFIRTree.EnvTree? = null
    var data: CobolFIRTree.DataTree? = null
    var procedure: CobolFIRTree.ProcedureTree? = null

    for (child in children) {
        if (child is PsiErrorElement) {
            error("$child")
        }
        if (child is PsiWhiteSpace) {
            continue
        }
        child as CobolProgram
        id = child.idDiv.toID()
        env = child.envDiv?.toEnv()
        data = child.dataDiv?.toData()
        procedure = child.procedureDiv.toProcedure(data)

        fileComments = child.comments.asComments()
    }
    if (id != null && procedure != null) {
        return CobolFIRTree(
            id = id, env = env, data = data, procedure = procedure,
            fileComments = fileComments ?: emptyList()
        )
    }
    error("No valid CobolLine found")
}

private val commentTokens = TokenSet.create(CobolTypes.COMMENT)

private fun PsiElement.asComments(): List<String> = node.getChildren(commentTokens).map {
    it.text.drop(1).trim()
}

private fun List<CobolComments>.asComments() = mapNotNull {
    val text = it.text
    if (text.startsWith("*")) {
        it.text.drop(1)
    } else null
}

private fun CobolIdDiv.toID(): CobolFIRTree.ID {
    val (programID, programmIDComments) = programID.let { it.varName.text to it.comments.asComments() }
    val (author, authorComments) = authorList.singleOrNull().let { it?.anys?.asString() to it?.comments?.asComments() }
    val (installation, installationComments) = installationList.singleOrNull()
        .let { it?.anys?.asString() to it?.comments?.asComments() }
    val (date, dateComments) = dateList.singleOrNull().let { it?.anys?.asString() to it?.comments?.asComments() }

    return CobolFIRTree.ID(
        programID = programID,
        programIDComments = programmIDComments,
        author = author,
        authorComments = authorComments ?: emptyList(),
        installation = installation,
        installationsComments = installationComments ?: emptyList(),
        date = date,
        dateComments = dateComments ?: emptyList()
    )
}

private fun CobolAnys.asString(): String {
    return siblings().joinToString("")
}

private fun CobolEnvDiv.toEnv(): CobolFIRTree.EnvTree = CobolFIRTree.EnvTree(
    configuration = config?.let {
        CobolFIRTree.EnvTree.Configuration(
            specialNames = it.specialNames?.let {
                CobolFIRTree.EnvTree.SpecialNames(
                    specialNames = it.specialNameDeclarationList.map {
                        CobolFIRTree.EnvTree.SpecialName(
                            env = it.specialName.specialNameEnv.text,
                            value = it.specialName.specialNameValue.text,
                            comments = it.comments.asComments()
                        )
                    },
                    comments = it.comments.asComments()
                )
            },
            comments = it.comments.asComments()
        )
    },
    comments = this.commentsList.asComments()
)

private fun CobolDataDiv.toData() = CobolFIRTree.DataTree(workingStorageSection?.saList?.map {
    val pic = it.pic
    when {
        pic.pic9 != null -> TODO()
        pic.picS != null -> TODO()
        pic.picXA != null -> CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar(
            name = it.varName.text,
            length = it.pic.number?.text?.toInt() ?: 1,
            value = it.`var`?.let {
                it.string!!.text!!.drop(1).dropLast(1)
            }, comments = it.comments.asComments()
        )

        else -> TODO()
    }
} ?: emptyList(),
    comments = commentsList.asComments()
)

private fun CobolProcedureDiv.toProcedure(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree {
    return CobolFIRTree.ProcedureTree(
        sentenceList.flatMap {
            it.proceduresList.map { it.asStatements(dataTree) }
        },
        procedureSectionList.map {
            CobolFIRTree.ProcedureTree.Section(
                name = it.varName.text,
                statements = it.sentence.proceduresList.map {
                    it.asStatements(dataTree)
                },
                comments = it.comments.asComments()
            )
        },
        comments = comments.asComments()
    )
}

private fun CobolProcedures.asStatements(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Statement {
    return when {
        display != null -> CobolFIRTree.ProcedureTree.Statement.Display(
            expr = display!!.stringConcat.toExpr(dataTree),
            comments = comments.asComments(),
        )

        moving != null -> CobolFIRTree.ProcedureTree.Statement.Move(
            target = dataTree.notNull.find(moving!!.varName), value = moving!!.expr.toExpr(dataTree),
            comments = comments.asComments()
        )

        performing != null -> CobolFIRTree.ProcedureTree.Statement.Perform(
            sectionName = performing!!.varName.text,
            comments = comments.asComments()
        )

        else -> TODO()
    }
}

private val CobolFIRTree.DataTree?.notNull
    get() = checkNotNull(this) {
        "No DATA DIVISION found"
    }

private fun CobolExpr.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression {
    val `var` = `var`
    val varName = varName
    val stringConcat = stringConcat
    return when {
        `var` != null -> when {
            `var`.string != null -> `var`.string!!.singleAsString(dataTree)
            `var`.number != null -> TODO()
            else -> TODO("$`var`")
        }

        varName != null -> dataTree.notNull.find(varName).toVariable()

        stringConcat != null -> stringConcat.toExpr(dataTree)
        else -> TODO("$elementType")
    }
}

private fun PsiElement.singleAsString(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    return when (elementType) {
        CobolTypes.STRING -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral(
            value = text.drop(1).dropLast(1)
        )

        CobolTypes.VARNAME -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
            dataTree.notNull.find(this) as CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar
        )

        else -> TODO("$elementType")
    }
}

private val CobolStringConcat.allChildren: Sequence<PsiElement>
    get() = generateSequence(firstChild) {
        it.firstChild ?: it.nextSibling
    }

private fun CobolStringConcat.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    val allChildren = allChildren.toList()
    require(allChildren.isNotEmpty())
    if (allChildren.count() == 1) {
        val single = allChildren.single()
        return single.singleAsString(dataTree)
    }

    val first = allChildren.first().singleAsString(dataTree)
    val s = allChildren.foldSecond(first) { acc, psi ->
        if (psi.elementType == TokenType.WHITE_SPACE) null
        else CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat(
            left = acc, right = psi.singleAsString(dataTree)
        )
    }
    return s
}

inline fun <T, R> Iterable<T>.foldSecond(initial: R, operation: (acc: R, T) -> R?): R {
    var accumulator = initial
    var first = true
    for (element in this) {
        if (first) {
            first = false
        } else {
            val next = operation(accumulator, element)
            if (next != null) {
                accumulator = next
            }
        }
    }
    return accumulator
}

private fun CobolFIRTree.DataTree.find(varName: PsiElement): CobolFIRTree.DataTree.WorkingStorage.Elementar {
    val name: String = varName.text
    return workingStorage.find { (it as? CobolFIRTree.DataTree.WorkingStorage.Elementar)?.name == name } as? CobolFIRTree.DataTree.WorkingStorage.Elementar
        ?: error("Elementar $name not found")
}

private fun CobolFIRTree.DataTree.WorkingStorage.Elementar.toVariable(): CobolFIRTree.ProcedureTree.Expression.Variable =
    when (this) {
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
            target = this
        )
    }
