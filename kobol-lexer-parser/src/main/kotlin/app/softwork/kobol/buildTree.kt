package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.EnvTree.Configuration.*
import app.softwork.kobol.CobolFIRTree.EnvTree.InputOutput.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
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
    val (programID, programmIDComments) = programIDClause.let { it.programIDID.varName.text to it.comments.asComments() }
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
                        file = it.fileConfigSelect.fileID.varName.text,
                        fileVariable = it.fileConfigAssign.fileAssignID.varName.text,
                        fileStatus = it.fileConfigStatus.fileStatusID.varName.text,
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
                            val name = record.recordID?.varName?.text
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
    val name = it.recordID?.varName?.text ?: return null
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
    val dependOn = occursClause.recordID?.varName?.text

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
        it.proceduresList.asStatements(dataTree)
    },
    sections = procedureSectionList.map {
        Section(
            name = it.sectionID.varName.text,
            statements = it.sentences.proceduresList.asStatements(dataTree),
            comments = it.comments.asComments()
        )
    },
    comments = comments.asComments()
)

private fun List<CobolProcedures>.asStatements(dataTree: CobolFIRTree.DataTree?): List<Statement> = flatMap { proc ->
    when {
        proc.displaying != null -> listOf(
            Display(
                expr = proc.displaying!!.stringConcat.toExpr(dataTree),
                comments = proc.comments.asComments(),
            )
        )

        proc.moving != null -> proc.moving!!.variableList.map {
            Move(
                target = find(it, dataTree.notNull),
                value = proc.moving!!.expr.toExpr(dataTree),
                comments = proc.comments.asComments()
            )
        }

        proc.performing != null -> {
            val doWhile = proc.performing!!.doWhile
            val forEach = proc.performing!!.forEach
            if (doWhile != null) {
                listOf(
                    Perform(
                        sectionName = doWhile.sectionID.varName.text,
                        comments = proc.comments.asComments(),
                        until = doWhile.booleanExpr?.toFir(dataTree)
                    )
                )
            } else if (forEach != null) {
                listOf(
                    ForEach(
                        variable = Expression.NumberExpression.NumberVariable.Local(
                            name = forEach.variable.varName.text
                        ),
                        from = forEach.number.text.toInt(),
                        to = forEach.forEachTo?.number?.text?.toInt(),
                        by = forEach.forEachBy?.number?.text?.toInt(),
                        until = forEach.booleanExpr.toFir(dataTree),
                        action = forEach.proceduresList.asStatements(dataTree)
                    )
                )
            } else notPossible()
        }

        proc.ctrl != null -> {
            val ctrl = proc.ctrl!!
            when (ctrl.firstChild.elementType) {
                CobolTypes.GOBACK -> listOf(GoBack(proc.comments.asComments()))
                CobolTypes.CONTINUE -> listOf(Continue(proc.comments.asComments()))
                else -> TODO()
            }
        }

        proc.calling != null -> {
            val calling = proc.calling!!

            val target = calling.callingName.callingNameProgramID?.text
            requireNotNull(target) {
                "Non hard-coded CALL is not supported due to compiler and linker limitations."
            }
            listOf(
                Call(
                    name = target.drop(1).dropLast(1),
                    parameters = calling.exprList.map { it.toExpr(dataTree) },
                    comments = proc.comments.asComments()
                )
            )
        }

        else -> TODO()
    }
}

private fun List<CobolProcedures>.find(variable: CobolVariable, dataTree: CobolFIRTree.DataTree): Elementar {
    for (proc in this) {
        val perf = proc.performing
        if (perf != null) {
            val forEach = perf.forEach
            if (forEach != null && forEach.variable.varName.text == variable.text) {
                return NumberElementar(
                    name = variable.text,
                    formatter = Formatter.Local
                )
            }
        }
    }
    return dataTree.find(variable)
}

private fun CobolBooleanExpr.toFir(dataTree: CobolFIRTree.DataTree?): BooleanExpression {
    val or = booleanExprOr
    val and = booleanExprAnd
    val clause = booleanExprClause
    return when {
        or != null -> BooleanExpression.Or(or.booleanExprClause.toFir(dataTree), or.booleanExpr.toFir(dataTree))
        and != null -> BooleanExpression.And(and.booleanExprClause.toFir(dataTree), and.booleanExpr.toFir(dataTree))
        clause != null -> clause.toFir(dataTree)
        else -> notPossible()
    }
}

private fun CobolBooleanExprClause.toFir(dataTree: CobolFIRTree.DataTree?): BooleanExpression {
    val left = booleanExprClauseLeft.expr.toExpr(dataTree)
    val right = booleanExprClauseRight.expr.toExpr(dataTree)

    val nt = booleanExprClauseNt
    val bigger = booleanExprClauseBigger
    val smaller = booleanExprClauseSmaller
    return when {
        nt != null -> {
            val equal = BooleanExpression.Equals(
                left = left,
                right = right
            )
            if (nt.nt != null) {
                BooleanExpression.Not(equal)
            } else equal
        }

        bigger != null -> BooleanExpression.Greater(
            left = left as Expression.NumberExpression,
            right = right as Expression.NumberExpression,
            equals = bigger.eql != null
        )

        smaller != null -> BooleanExpression.Smaller(
            left = left as Expression.NumberExpression,
            right = right as Expression.NumberExpression,
            equals = smaller.eql != null
        )

        else -> notPossible()
    }
}

private val CobolFIRTree.DataTree?.notNull
    get() = checkNotNull(this) {
        "No DATA DIVISION found"
    }

private fun CobolExpr.toExpr(dataTree: CobolFIRTree.DataTree?): Expression {
    val literal = literal
    val variable = variable
    val stringConcat = stringConcat
    return when {
        literal != null -> when {
            literal.string != null -> literal.string!!.singleAsString(dataTree)
            literal.number != null -> TODO()
            else -> TODO("$literal")
        }

        variable != null -> dataTree.notNull.find(variable).toVariable()

        stringConcat != null -> stringConcat.toExpr(dataTree)
        else -> TODO("$elementType")
    }
}

private fun PsiElement.singleAsString(dataTree: CobolFIRTree.DataTree?): Expression.StringExpression {
    return when {
        elementType == CobolTypes.STRING || elementType == CobolTypes.STRING_VAR -> Expression.StringExpression.StringLiteral(
            value = text.drop(1).dropLast(1)
        )

        this is CobolVariable -> {
            Expression.StringExpression.StringVariable(
                dataTree.notNull.find(this) as StringElementar
            )
        }

        else -> TODO("$elementType")
    }
}

private fun CobolStringConcat.toExpr(dataTree: CobolFIRTree.DataTree?): Expression.StringExpression {
    val allChildren = children.toList()
    require(allChildren.isNotEmpty())
    if (allChildren.count() == 1) {
        val single = allChildren.single()
        return single.singleAsString(dataTree)
    }

    val first = allChildren.first().singleAsString(dataTree)
    val s = allChildren.foldSecond(first) { acc, psi ->
        if (psi.elementType == TokenType.WHITE_SPACE) null
        else Expression.StringExpression.Concat(
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

private fun CobolFIRTree.DataTree.find(variable: CobolVariable): Elementar {
    val name: String = variable.varName.text
    val all = findAll(variable)
    val of = variable.ofClause
    if (of == null) {
        val key = all.keys.singleOrNull()
        return all[key] ?: error("Elementar $name not found")
    } else {
        for ((key, element) in all) {
            if (key?.name == of.recordID.varName.text) {
                return element
            }
        }
        error("Elementar $name OF ${of.recordID.varName.text} not found")
    }
}

fun CobolFIRTree.DataTree.findAll(varName: CobolVariable): Map<Record?, Elementar> {
    val name: String = varName.varName.text
    return buildMap {
        for (element in workingStorage) {
            when (element) {
                is Elementar -> if (element.name == name) {
                    this[null] = element
                }

                is Record -> for (elementar in element.elements) {
                    if (elementar.name == name) {
                        this[element] = elementar
                    }
                }
            }
        }
    }
}

private fun Elementar.toVariable(): Expression.Variable = when (this) {
    is StringElementar -> Expression.StringExpression.StringVariable(
        target = this
    )

    is NumberElementar -> TODO()
    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}

fun notPossible(): Nothing = error("Should not be possible!")
