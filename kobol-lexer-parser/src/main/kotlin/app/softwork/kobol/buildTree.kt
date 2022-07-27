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

fun PsiElement.asComments(): List<String> = node.getChildren(commentTokens).map {
    it.text.drop(1).trim()
}

private fun List<CobolComments>.asComments() = mapNotNull {
    val text = it.text
    if (text.startsWith("*")) {
        it.text.drop(1)
    } else null
}

private fun CobolIdDiv.toID(): CobolFIRTree.ID {
    val (programID, programmIDComments) = programIDClause.let { it.id.varName.text to it.comments.asComments() }
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
    return siblings().joinToString("") {
        it.text
    }.removePrefix(".").trim()
}

private fun CobolEnvDiv.toEnv(): CobolFIRTree.EnvTree = CobolFIRTree.EnvTree(configuration = config?.let {
    CobolFIRTree.EnvTree.Configuration(
        specialNames = it.specialNamesDef?.let {
            SpecialNames(
                specialNames = it.specialNameDeclarationList.map {
                    SpecialNames.SpecialName(
                        env = it.specialNameEnv.text,
                        value = it.specialNameValue.text,
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
                        file = it.fileConfigSelect.id.varName.text,
                        fileVariable = it.fileConfigAssign.id.varName.text,
                        fileStatus = it.fileConfigStatus.id.varName.text,
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
                            val name = record.id?.varName?.text
                            if (name != null) {
                                currentRecord = Record(name, emptyList())
                            }
                        }

                        77 -> {
                            if (currentRecord != null) {
                                add(currentRecord)
                            }
                            currentRecord = null
                            val sa = sa(record, emptyList())
                            if (sa != null) {
                                add(sa)
                            }
                        }

                        else -> {
                            requireNotNull(currentRecord)
                            val elementar = sa(record, currentRecord.elements.filterIsInstance<NumberElementar>())
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

private fun sa(it: CobolRecordDef, previous: List<NumberElementar>): Elementar? {
    val name = it.id?.varName?.text ?: return null
    val pic: List<CobolPicDefClause>? = it.picClause?.picDefClauseList?.takeIf { it.isNotEmpty() }
    val single = pic?.singleOrNull()
    val value = it.picClause?.literal?.text
    return when {
        pic == null -> {
            if (it.pointerClause != null) {
                Pointer(name, it.comments.asComments())
            } else {
                EmptyElementar(name, it.comments.asComments())
            }
        }

        single != null -> single(
            single,
            value = value,
            name = name,
            comments = it.comments,
            occurs = it.picClause!!.occursClauseList,
            previous = previous
        )

        else -> {
            val first = pic.first()
            val formatter = pic.asFormat()
            single(
                first,
                formatter,
                value = value,
                name = name,
                comments = it.comments,
                occurs = it.picClause!!.occursClauseList,
                previous = previous
            )
        }
    }
}

// https://www.ibm.com/docs/en/developer-for-zos/9.1.1?topic=clause-symbols-used-in-picture
private fun List<CobolPicDefClause>.asFormat(): Formatter.Custom = Formatter.Custom(map {
    when (val text = it.pictures.text) {
        "S9" -> Formatter.Custom.Part.Signed(it.length())
        "9" -> Formatter.Custom.Part.Number(it.length())
        "X", "A" -> Formatter.Custom.Part.String(it.length())
        "B" -> Formatter.Custom.Part.Space(it.length())
        "Z" -> Formatter.Custom.Part.Zero(it.length())
        "+" -> Formatter.Custom.Part.Plus(it.length())
        "V9" -> Formatter.Custom.Part.Decimal(it.length())
        else -> error("Not yet supported: $text")
    }
})

fun CobolPicDefClause.length() = length?.number?.text?.toInt() ?: 1

// https://www.ibm.com/docs/en/cobol-zos/6.1?topic=clause-occurs-depending
private fun List<CobolOccursClause>.toFir(previous: List<NumberElementar>): Occurs? {
    val occursClause = singleOrNull() ?: return null
    val from = occursClause.occursClauseNumber.number.text.toInt()
    val to = occursClause.occursClauseNumberTo?.number?.text?.toInt()
    val dependOn = occursClause.id?.varName?.text

    return Occurs(
        from = from,
        to = to,
        dependingOn = dependOn?.let {
            previous.find {
                it.name == dependOn
            }
        },
    )
}

private fun single(
    single: CobolPicDefClause,
    format: Formatter = Formatter.Simple(single.length()),
    value: String?,
    comments: CobolComments,
    name: String,
    occurs: List<CobolOccursClause>,
    previous: List<NumberElementar>
): Elementar = when (val p = single.pictures.text) {
    "A", "X" -> StringElementar(
        name = name,
        formatter = format,
        value = value?.drop(1)?.dropLast(1),
        comments = comments.asComments(),
        occurs = occurs.toFir(previous)
    )

    "9" -> {
        NumberElementar(
            name = name,
            formatter = format,
            value = value?.toDouble(),
            comments = comments.asComments(),
            occurs = occurs.toFir(previous)
        )
    }

    "S9" -> NumberElementar(
        name = name,
        formatter = format,
        value = value?.toDouble(),
        signed = true,
        comments = comments.asComments(),
        occurs = occurs.toFir(previous)
    )

    "V9" -> NumberElementar(
        name = name,
        formatter = format,
        value = value?.toDouble(),
        signed = true,
        comments = comments.asComments(),
        occurs = occurs.toFir(previous)
    )

    else -> TODO(p)
}

private fun CobolProcedureDiv.toProcedure(dataTree: CobolFIRTree.DataTree?) = CobolFIRTree.ProcedureTree(
    topLevel = sentencesList.flatMap {
        it.proceduresList.flatMap {
            it.asStatements(dataTree)
        }
    },
    sections = procedureSectionList.map {
        CobolFIRTree.ProcedureTree.Section(
            name = it.id.varName.text,
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
                        sectionName = doWhile.id.varName.text, comments = comments.asComments()
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
    val varName = variable?.id?.varName
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
            id.varName.singleAsString(dataTree)
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

    is NumberElementar -> TODO()
    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}
