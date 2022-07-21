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
    val (programID, programmIDComments) = programIDClause.let { it.varName.text to it.comments.asComments() }
    val (author, authorComments) = authorClauseList.singleOrNull()
        .let { it?.anys?.asString() to it?.comments?.asComments() }
    val (installation, installationComments) = installationClauseList.singleOrNull()
        .let { it?.anys?.asString() to it?.comments?.asComments() }
    val (date, dateComments) = dateClauseList.singleOrNull().let { it?.anys?.asString() to it?.comments?.asComments() }

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
        specialNames = it.specialNamesDef?.let {
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
}, inputOutput = inputSection?.let {
    CobolFIRTree.EnvTree.InputOutput(
        fileControl = it.fileControlClause?.let {
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
                val record = stm.recordDef
                val sql = stm.execSqlDef
                if (record != null) {
                    when (record.number.text.toInt()) {
                        1 -> {
                            if (currentRecord != null) {
                                add(currentRecord)
                            }
                            val name = record.varName?.text
                            if (name != null) {
                                currentRecord = Record(name, emptyList())
                            }
                        }

                        77 -> {
                            if (currentRecord != null) {
                                add(currentRecord)
                            }
                            currentRecord = null
                            val sa = sa(record)
                            if (sa != null) {
                                add(sa)
                            }
                        }

                        else -> {
                            requireNotNull(currentRecord)
                            val elementar = sa(record)
                            if (elementar != null) {
                                currentRecord = currentRecord.copy(elements = currentRecord.elements + elementar)
                            }
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

private fun sa(it: CobolRecordDef): Elementar? {
    val name = it.varName?.text ?: return null
    val pic: List<CobolPicDefClause>? = it.picClause?.picDefClauseList
    val single = pic?.singleOrNull()
    return when {
        pic == null -> TODO()
        single != null -> when (single.pictures.text) {
            "A", "X" -> StringElementar(
                name = name, length = single.length?.number?.text?.toInt() ?: 1, value = it.picClause?.literal?.let {
                    it.text!!.drop(1).dropLast(1)
                }, comments = it.comments.asComments()
            )

            else -> TODO()
        }

        else -> TODO()
    }
}

private fun CobolProcedureDiv.toProcedure(dataTree: CobolFIRTree.DataTree?) = CobolFIRTree.ProcedureTree(
    topLevel = sentencesList.flatMap {
        it.proceduresList.flatMap {
            it.asStatements(dataTree)
        }
    },
    sections = procedureSectionList.map {
        CobolFIRTree.ProcedureTree.Section(
            name = it.varName.text,
            statements = it.sentencesList.flatMap {
                it.proceduresList.flatMap {
                    it.asStatements(dataTree)
                }
            },
            comments = it.comments.asComments()
        )
    },
    comments = comments.asComments()
)

private fun CobolProcedures.asStatements(dataTree: CobolFIRTree.DataTree?): List<CobolFIRTree.ProcedureTree.Statement> {
    return when {
        displaying != null -> listOf(
            CobolFIRTree.ProcedureTree.Statement.Display(
                expr = displaying!!.stringConcat.toExpr(dataTree),
                comments = comments.asComments(),
            )
        )

        moving != null -> moving!!.variableList.map {
            CobolFIRTree.ProcedureTree.Statement.Move(
                target = dataTree.notNull.find(it),
                value = moving!!.expr.toExpr(dataTree),
                comments = comments.asComments()
            )
        }

        performing != null -> {
            val doWhile = performing!!.doWhile
            if (doWhile != null) {
                listOf(
                    CobolFIRTree.ProcedureTree.Statement.Perform(
                        sectionName = doWhile.varName.text, comments = comments.asComments()
                    )
                )
            } else {
                TODO()
            }
        }

        else -> TODO()
    }
}

private val CobolFIRTree.DataTree?.notNull
    get() = checkNotNull(this) {
        "No DATA DIVISION found"
    }

private fun CobolExpr.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression {
    val literal = literal
    val varName = variable?.varName
    val stringConcat = stringConcat
    return when {
        literal != null -> when {
            literal.string != null -> literal.string!!.singleAsString(dataTree)
            literal.number != null -> TODO()
            else -> TODO("$literal")
        }

        varName != null -> dataTree.notNull.find(varName).toVariable()

        stringConcat != null -> stringConcat.toExpr(dataTree)
        else -> TODO("$elementType")
    }
}

private fun PsiElement.singleAsString(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    return when (elementType) {
        CobolTypes.STRING, CobolTypes.STRING_VAR -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral(
            value = text.drop(1).dropLast(1)
        )

        CobolTypes.VARNAME -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
            dataTree.notNull.find(this) as StringElementar
        )

        CobolTypes.VARIABLE -> {
            this as CobolVariable
            this.varName.singleAsString(dataTree)
        }

        else -> TODO("$elementType")
    }
}

private fun CobolStringConcat.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    val allChildren = children.toList()
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
