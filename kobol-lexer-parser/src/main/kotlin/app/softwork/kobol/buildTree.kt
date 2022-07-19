package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.EnvTree.Configuration.*
import app.softwork.kobol.CobolFIRTree.EnvTree.InputOutput.*
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
            id = id, env = env, data = data, procedure = procedure, fileComments = fileComments ?: emptyList()
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

private fun CobolEnvDiv.toEnv(): CobolFIRTree.EnvTree = CobolFIRTree.EnvTree(configuration = config?.let {
    CobolFIRTree.EnvTree.Configuration(
        specialNames = it.specialNames?.let {
            SpecialNames(
                specialNames = it.specialNameDeclarationList.map {
                    SpecialNames.SpecialName(
                        env = it.specialName.specialNameEnv.text,
                        value = it.specialName.specialNameValue.text,
                        comments = it.comments.asComments()
                    )
                }, comments = it.comments.asComments()
            )
        }, comments = it.comments.asComments()
    )
}, inputOutput = input?.let {
    CobolFIRTree.EnvTree.InputOutput(
        fileControl = it.fileControl?.let {
            FileControl(
                it.fileConfigList.map {
                    FileControl.File(
                        file = it.fileConfigSelect.varName.text,
                        fileVariable = it.fileConfigAssign.varName.text,
                        fileStatus = it.fileConfigStatus.varName.text,
                        comments = it.comments.asComments()
                    )
                }, comments = it.comments.asComments()
            )
        }, comments = it.comments.asComments()
    )
}, comments = comments.asComments()
)

private fun CobolDataDiv.toData(): CobolFIRTree.DataTree {
    val definitions = workingStorageSection?.stmList?.toMutableList()?.let {
        buildList<CobolFIRTree.DataTree.WorkingStorage> {
            var currentRecord: Record? = null
            for (stm in it) {
                val record = stm.record
                val sql = stm.execSql
                if (record != null) {
                    when (val number = record.number.text.toInt()) {
                        1 -> {
                            if (currentRecord != null) {
                                add(currentRecord)
                            }
                            currentRecord = Record(record.varName.text, emptyList())
                        }

                        77 -> {
                            if (currentRecord != null) {
                                add(currentRecord)
                            }
                            currentRecord = null
                            add(sa(record))
                        }

                        else -> {
                            requireNotNull(currentRecord)
                            val elementar = sa(record)
                            currentRecord = currentRecord.copy(elements = currentRecord.elements + elementar)
                        }
                    }
                } else {
                    TODO()
                }
            }
            if (currentRecord != null) {
                add(currentRecord)
            }
        }
    }

    return CobolFIRTree.DataTree(
        workingStorage = definitions ?: emptyList(), comments = commentsList.asComments()
    )
}

private fun sa(it: CobolRecord): Elementar {
    val pic = it.pic
    return when {
        pic == null -> TODO()
        pic.pic9 != null -> TODO()
        pic.picS9 != null -> TODO()
        pic.picXA != null -> StringElementar(
            name = it.varName.text, length = pic.number?.text?.toInt() ?: 1, value = it.`var`?.let {
                it.text!!.drop(1).dropLast(1)
            }, comments = it.comments.asComments()
        )

        else -> TODO()
    }
}

private fun CobolProcedureDiv.toProcedure(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree {
    return CobolFIRTree.ProcedureTree(sentenceList.flatMap {
        it.proceduresList.map { it.asStatements(dataTree) }
    }, procedureSectionList.map {
        CobolFIRTree.ProcedureTree.Section(
            name = it.varName.text, statements = it.sentence.proceduresList.map {
                it.asStatements(dataTree)
            }, comments = it.comments.asComments()
        )
    }, comments = comments.asComments()
    )
}

private fun CobolProcedures.asStatements(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Statement {
    return when {
        display != null -> CobolFIRTree.ProcedureTree.Statement.Display(
            expr = display!!.stringConcat.toExpr(dataTree),
            comments = comments.asComments(),
        )

        moving != null -> CobolFIRTree.ProcedureTree.Statement.Move(
            target = dataTree.notNull.find(moving!!.varName),
            value = moving!!.expr.toExpr(dataTree),
            comments = comments.asComments()
        )

        performing != null -> CobolFIRTree.ProcedureTree.Statement.Perform(
            sectionName = performing!!.varName.text, comments = comments.asComments()
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
            dataTree.notNull.find(this) as StringElementar
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

private fun CobolFIRTree.DataTree.find(varName: PsiElement): Elementar {
    val name: String = varName.text
    for (element in workingStorage) {
        when (element) {
            is Elementar -> if (element.name == name) {
                return element
            }

            is Record -> {
                for (elementar in element.elements) {
                    if (elementar.name == name) {
                        return elementar
                    }
                }
            }
        }
    }
    error("Elementar $name not found")
}

private fun Elementar.toVariable(): CobolFIRTree.ProcedureTree.Expression.Variable = when (this) {
    is StringElementar -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
        target = this
    )
}
