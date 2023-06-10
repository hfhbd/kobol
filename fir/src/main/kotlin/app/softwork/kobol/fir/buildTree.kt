package app.softwork.kobol.fir

import app.softwork.kobol.*
import app.softwork.kobol.fir.CobolFIRTree.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.fir.CobolFIRTree.EnvTree.Configuration.*
import app.softwork.kobol.fir.CobolFIRTree.EnvTree.InputOutput.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.sqldelight.db2dialect.grammar.psi.*
import com.alecstrong.sql.psi.core.*
import com.alecstrong.sql.psi.core.psi.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.util.*
import com.intellij.testFramework.*

public fun CobolFile.toTree(): CobolFIRTree {
    val errors = this.childrenOfType<PsiErrorElement>()
    if(errors.isNotEmpty()) {
        error(errors.joinToString(separator = "\n") { 
            it.errorDescription
        })
    }
    val id = program.idDiv.toID()
    val env = program.envDiv?.toEnv()
    val data = program.dataDiv?.toData(env) ?: returnCode
    val procedure = program.procedureDiv.toProcedure(data)

    val fileComments = program.comments.asComments()

    return CobolFIRTree(
        fileName = name, id = id, env = env, data = data, procedure = procedure, fileComments = fileComments
    )
}

internal val returnCode = DataTree(
    workingStorage = listOf(NumberElementar.ReturnCode())
)

private fun CobolIdDiv.toID(): ID {
    val (programID, programmIDComments) = programIDClause.let { it.programIDID.varName.text to it.comments.asComments() }
    val (author, authorComments) = authorClauseList.singleOrNull()
        .let { it?.anys?.asString() to it?.comments?.asComments() }
    val (installation, installationComments) = installationClauseList.singleOrNull()
        .let { it?.anys?.asString() to it?.comments?.asComments() }
    val (date, dateComments) = dateClauseList.singleOrNull().let { it?.anys?.asString() to it?.comments?.asComments() }

    return ID(
        programID = programID,
        programIDComments = programmIDComments,
        author = author,
        authorComments = authorComments ?: emptyList(),
        installation = installation,
        installationComments = installationComments ?: emptyList(),
        date = date,
        dateComments = dateComments ?: emptyList()
    )
}

private fun CobolAnys.asString(): String {
    return siblings().joinToString("") {
        it.text
    }.removePrefix(".").trim()
}

private fun CobolEnvDiv.toEnv(): EnvTree = EnvTree(configuration = config?.let {
    EnvTree.Configuration(
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
    EnvTree.InputOutput(
        fileControl = it.fileControlClause?.let {
            FileControl(
                it.fileConfigList.map {
                    val path: String = it.fileConfigAssign.fileAssignID.let {
                        if (it.varName != null) {
                            it.varName!!.text
                        } else it.string!!.text.drop(1).dropLast(1)
                    }
                    val type = if (it.fileLineSequential != null) {
                        File.FileType.LineSequential
                    } else File.FileType.Sequential
                    FileControl.File(
                        file = it.fileConfigSelect.fileID.varName.text,
                        path = path,
                        type = type,
                        fileStatus = it.fileConfigStatus?.fileStatusID?.varName?.text,
                        comments = it.comments.asComments()
                    )
                }, comments = it.comments.asComments()
            )
        }, comments = it.comments.asComments()
    )
}, comments = comments.asComments()
)

private fun MutableList<WorkingStorage>.record(
    record: CobolRecordDef, currentRecord: Record?
): Record? {
    var currentRecord1 = currentRecord
    when (record.number.text.toInt()) {
        1 -> {
            if (currentRecord1 != null) {
                add(currentRecord1)
            }
            val name = record.recordID?.varName?.text
            if (name != null) {
                currentRecord1 = Record(name, emptyList(), record.comments.asComments())
            }
        }

        77 -> {
            if (currentRecord1 != null) {
                add(currentRecord1)
            }
            currentRecord1 = null
            val sa = sa(record, recordName = null, emptyList())
            if (sa != null) {
                add(sa)
            }
        }

        else -> {
            requireNotNull(currentRecord1)
            val elementar = sa(record, currentRecord1.name, currentRecord1.elements.filterIsInstance<NumberElementar>())
            if (elementar != null) {
                currentRecord1 = currentRecord1.copy(elements = currentRecord1.elements + elementar)
            }
        }
    }
    return currentRecord1
}

private fun List<CobolRecordDef>.toRecords() = buildList {
    var currentRecord: Record? = null
    for (record: CobolRecordDef in this@toRecords) {
        currentRecord = record(record, currentRecord)
    }
    if (currentRecord != null) {
        add(currentRecord)
    }
} as List<Record>

private fun CobolDataDiv.toData(envTree: EnvTree?): DataTree {
    val definitions: List<WorkingStorage> = workingStorageSection?.stmList?.let {
        buildList {
            var currentRecord: Record? = null
            for (stm in it) {
                val record = stm.recordDef
                val sql = stm.execSqlDef

                if (record != null) {
                    currentRecord = record(record, currentRecord)
                } else if (sql != null) {
                    val sqlString = sql.execSql.children.joinToString("") {
                        it.text
                    }.trim().replace("\n ", "\n")
                    val include = """INCLUDE (.*)""".toRegex()
                    if (sqlString.contains(include)) {
                        include.findAll(sqlString).forEach {
                            val toInclude = it.groups[1]!!.value
                            for (record in CobolElementFactory.includeSQL(project, toInclude)) {
                                currentRecord = record(record, currentRecord)
                            }
                        }
                    } else {
                        var comments = sql.comments.asComments()
                        for (sqlStmt in checkSql(sqlString, project)) {
                            val tableComments = comments
                            comments = emptyList()
                            add(
                                WorkingStorage.Sql(
                                    sql = sqlStmt.text, comments = tableComments
                                )
                            )
                        }
                    }
                }
            }
            currentRecord?.let { add(it) }

            add(NumberElementar.ReturnCode())
        }
    } ?: listOf(NumberElementar.ReturnCode())
    val linkage = linkingSection?.recordDefList?.toRecords()

    val fileSection = fileSection?.let {
        val files = requireNotNull(requireNotNull(requireNotNull(envTree).inputOutput).fileControl).files

        it.fileDescriptionsList.map {
            val description = it.fileDescription
            val name: String = description.fileDescriptionID.varName.text
            val file = files.single { it.file == name }
            File(
                name = name,
                description = description.let {
                    var blocks: Int? = null
                    var recording: String? = null
                    var records: IntRange? = null

                    for (desc in it.fileDescriptorsList) {
                        when {
                            desc.blockClause != null -> blocks = desc.blockClause!!.number.text.toInt()
                            desc.recordingClause != null -> recording = desc.recordingClause!!.varName.text
                            desc.fileRecord != null -> records = desc.fileRecord?.number?.text?.toInt()?.let { start ->
                                val other = desc.fileRecord!!.fileRecordTo?.number?.text?.toInt() ?: start
                                start.rangeTo(other)
                            }
                        }
                    }

                    File.FileDescription(
                        recording = recording, blocks = blocks, records = records, comments = it.comments.asComments()
                    )
                },
                records = it.recordDefList.toRecords(),
                fileStatus = file.fileStatus,
                filePath = file.path,
                type = file.type
            )
        }
    }

    return DataTree(
        fileSection = fileSection ?: emptyList(),
        workingStorage = definitions,
        linkingSection = linkage ?: emptyList(),
        comments = comments.asComments()
    )
}

private fun DataTree.findFile(fileName: String): File = fileSection.single { it.name == fileName }

private fun DataTree.findFile(fileRecord: Record): File =
    fileSection.single { it.recordName == fileRecord.name }

private fun sa(it: CobolRecordDef, recordName: String?, previous: List<NumberElementar>): Elementar? {
    val name = it.recordID?.varName?.text ?: return null
    val pic: List<CobolPicDefClause>? = it.picClause?.picDefClauseList?.takeIf { it.isNotEmpty() }
    val single = pic?.singleOrNull()
    val value = it.picClause?.literal?.text
    return when {
        pic == null -> {
            if (it.pointerClause != null) {
                Pointer(name, recordName, it.comments.asComments())
            } else {
                EmptyElementar(name, recordName, it.comments.asComments())
            }
        }

        single != null -> single(
            single.pictures.text,
            format = Formatter.Simple(single.length()),
            value = value,
            name = name,
            recordName = recordName,
            comments = it.comments,
            occurs = it.picClause!!.occursClauseList,
            previous = previous,
            compressed = it.picClause!!.compressed?.text
        )

        else -> {
            val first = pic.first()
            val formatter = pic.asFormat()
            single(
                first.pictures.text,
                formatter,
                value = value,
                name = name,
                recordName = recordName,
                comments = it.comments,
                occurs = it.picClause!!.occursClauseList,
                previous = previous,
                compressed = it.picClause!!.compressed?.text
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

private fun CobolPicDefClause.length() = length?.number?.text?.toInt() ?: 1

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
    type: String,
    format: Formatter,
    value: String?,
    comments: CobolComments,
    name: String,
    recordName: String?,
    occurs: List<CobolOccursClause>,
    previous: List<NumberElementar>,
    compressed: String?
): Elementar = when (type.uppercase()) {
    "A", "X" -> StringElementar(
        name = name,
        recordName = recordName,
        formatter = format,
        value = value?.drop(1)?.dropLast(1),
        comments = comments.asComments(),
        occurs = occurs.toFir(previous),
        synthetic = false
    )

    "9" -> {
        Elementar.NumberElementar.Normal(
            name = name,
            recordName = recordName,
            formatter = format,
            value = value?.let {
                if (it.startsWith("ZERO")) {
                    0.0
                } else it.toDouble()
            },
            comments = comments.asComments(),
            occurs = occurs.toFir(previous),
            compressed = compressed?.let {
                NumberElementar.Compressed.valueOf(it.replace("-", ""))
            })
    }

    "S9" -> NumberElementar.Normal(name = name,
        recordName = recordName,
        formatter = format,
        value = value?.let {
            if (it.startsWith("ZERO")) {
                0.0
            } else it.toDouble()
        },
        signed = true,
        comments = comments.asComments(),
        occurs = occurs.toFir(previous),
        compressed = compressed?.let {
            NumberElementar.Compressed.valueOf(it.replace("-", ""))
        })

    "V9" -> NumberElementar.Normal(name = name,
        recordName = recordName,
        formatter = format,
        value = value?.let {
            if (it.startsWith("ZERO")) {
                0.0
            } else it.toDouble()
        },
        signed = false,
        comments = comments.asComments(),
        occurs = occurs.toFir(previous),
        compressed = compressed?.let {
            NumberElementar.Compressed.valueOf(it.replace("-", ""))
        })

    else -> TODO(type)
}

private fun CobolProcedureDiv.toProcedure(dataTree: DataTree) = ProcedureTree(topLevel = sentencesList.flatMap {
    it.proceduresList.asStatements(dataTree)
}, sections = procedureSectionList.map {
    Section(
        name = it.sectionID.varName.text,
        statements = it.sentences.proceduresList.asStatements(dataTree),
        comments = it.comments.asComments()
    )
}, comments = comments.asComments()
)

private fun List<CobolProcedures>.asStatements(dataTree: DataTree): List<Statement> = flatMap { proc ->
    when {
        proc.displaying != null -> listOf(
            Display(
                expr = proc.displaying!!.stringConcat.toExpr(dataTree),
                comments = proc.comments.asComments(),
            )
        )

        proc.moving != null -> proc.moving!!.variableList.map {
            Move(
                target = dataTree.find(it) as Elementar,
                value = proc.moving!!.expr.toExpr(dataTree).single(),
                comments = proc.comments.asComments()
            )
        }

        proc.adding != null -> proc.adding!!.variableList.map {
            Add(
                target = dataTree.find(it) as Elementar,
                value = proc.adding!!.expr.toExpr(dataTree).single(),
                comments = proc.comments.asComments()
            )
        }

        proc.subtracting != null -> listOf(
            Sub(
                target = dataTree.find(proc.subtracting!!.variable) as Elementar,
                value = proc.subtracting!!.expr.toExpr(dataTree).single(),
                comments = proc.comments.asComments()
            )
        )

        proc.performing != null -> {
            val isWhile = proc.performing!!.`while`
            val performWhile = proc.performing!!.performWhile
            val forEach = proc.performing!!.forEach
            if (performWhile != null) {
                listOf(
                    Perform(
                        sectionName = performWhile.sectionID.varName.text,
                        comments = proc.comments.asComments(),
                        until = performWhile.booleanExpr?.toFir(dataTree),
                        // https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statement-perform-varying-phrase
                        testing = performWhile.asDoWhile?.afterWhile?.let { 
                            Perform.Testing.After
                        } ?: Perform.Testing.Before
                    )
                )
            } else if (forEach != null) {
                listOf(
                    ForEach(
                        variable = dataTree.find(forEach.variable) as NumberElementar,
                        from = forEach.expr.toExpr(dataTree).single() as Expression.NumberExpression,
                        by = forEach.forEachBy?.expr?.toExpr(dataTree)?.single() as Expression.NumberExpression?,
                        until = forEach.booleanExpr.toFir(dataTree),
                        statements = forEach.proceduresList.asStatements(dataTree),
                        comments = proc.comments.asComments()
                    )
                )
            } else if (isWhile != null) {
                listOf(
                    While(
                        statements = isWhile.proceduresList.asStatements(dataTree),
                        until = isWhile.booleanExpr.toFir(dataTree),
                        comments = proc.comments.asComments()
                    )
                )
            } else notPossible()
        }

        proc.ctrl != null -> {
            val ctrl = proc.ctrl!!
            when {
                ctrl.ctrlGoBack != null -> listOf(GoBack(proc.comments.asComments()))
                ctrl.ctrlContinue != null -> listOf(Continue(proc.comments.asComments()))
                ctrl.ctrlStopRun != null -> listOf(StopRun(proc.comments.asComments()))
                else -> TODO(ctrl.toString())
            }
        }

        proc.calling != null -> {
            val calling = proc.calling!!

            val target = calling.callingName.callingNameProgramID?.text
            requireNotNull(target) {
                "Non hard-coded CALL is not supported due to compiler and linker limitations."
            }
            val parameters = calling.callingParameter?.exprList?.flatMap {
                it.toExpr(dataTree)
            } ?: emptyList()
            listOf(
                Call(
                    name = target.drop(1).dropLast(1), parameters = parameters, comments = proc.comments.asComments()
                )
            )
        }

        proc.execSql != null -> {
            val sql = proc.execSql!!.children.joinToString("") {
                it.text
            }.trim().replace(" \n ", "\n")

            val sqlStmts = checkSql(sql, proc.project)
            sqlStmts.map {
                val hostVariables = it.asSequence().filter {
                    it is Db2HostVariableId
                }.map { it.text }.toList()

                val bindParameter = it.asSequence().filter {
                    it is SqlBindParameter
                }.map { it.text.drop(1) }.toList()

                val type = when {
                    it.insertStmt != null -> Statement.Sql.SqlType.Insert
                    it.compoundSelectStmt != null || (it.extensionStmt as? Db2ExtensionStmt) != null -> Statement.Sql.SqlType.Select
                    it.deleteStmtLimited != null -> Statement.Sql.SqlType.Delete
                    else -> Statement.Sql.SqlType.Execute
                }
                Statement.Sql(sql = it.text, comments = proc.comments.asComments(), hostVariables = hostVariables.map {
                    (dataTree.workingStorage.find(it, null) as Elementar).toVariable()
                }, parameter = bindParameter.map {
                    (dataTree.workingStorage.find(it, null) as Elementar).toVariable()
                }, type = type
                )
            }
        }

        proc.ifClause != null -> {
            val ifClause = proc.ifClause!!
            listOf(
                If(
                    condition = ifClause.booleanExpr.toFir(dataTree),
                    statements = ifClause.proceduresList.asStatements(dataTree),
                    elseStatements = ifClause.ifElse?.proceduresList?.asStatements(dataTree) ?: emptyList(),
                    comments = proc.comments.asComments()
                )
            )
        }

        proc.eval != null -> {
            val eval = proc.eval!!

            listOf(Eval(values = eval.exprList.flatMap { it.toExpr(dataTree) }, conditions = eval.whensList.map {
                Eval.Condition(
                    conditions = it.exprList.flatMap { it.toExpr(dataTree) },
                    action = it.proceduresList.asStatements(dataTree),
                    comments = it.comments.asComments()
                )
            }, other = eval.whenOther?.let {
                Eval.Other(
                    action = it.proceduresList.asStatements(dataTree), comments = it.comments.asComments()
                )
            }, comments = proc.comments.asComments()
            )
            )
        }

        proc.reading != null -> {
            val reading = proc.reading!!
            val file = dataTree.findFile(reading.fileDescriptionID.varName.text)

            listOf(
                Read(
                    file = file,
                    action = reading.readingDuring?.proceduresList?.asStatements(dataTree) ?: emptyList(),
                    atEnd = reading.readingEnd?.proceduresList?.asStatements(dataTree) ?: emptyList(),
                    comments = proc.comments.asComments()
                )
            )
        }

        proc.writing != null -> {
            val writing = proc.writing!!
            val fileRecord = dataTree.find(writing.variable) as Record
            val file = dataTree.findFile(fileRecord)
            val from = writing.writingFrom?.variable?.let {
                dataTree.find(it)
            }

            listOf(
                Write(
                    file = file, from = from, comments = proc.comments.asComments()
                )
            )
        }

        proc.closing != null -> listOf(
            Close(
                files = proc.closing!!.fileDescriptionIDList.map {
                    dataTree.findFile(it.varName.text)
                },
                comments = proc.comments.asComments()
            )
        )

        proc.opening != null -> {
            val opening = proc.opening!!
            listOf(
                Open(
                    file = dataTree.findFile(opening.fileDescriptionID.varName.text),
                    type = when (val type = opening.openingType.text.lowercase()) {
                        "input" -> Open.Type.Input
                        "output" -> Open.Type.Output
                        else -> error("Unsupported $type")
                    },
                    comments = proc.comments.asComments()
                )
            )
        }

        else -> TODO(proc.toString())
    }
}

private fun checkSql(sql: String, project: Project): List<SqlStmt> {
    val file = LightVirtualFile("inlineSql/sql.inlinesql", SqlInlineLanguage, sql)
    val sqlFile = PsiManager.getInstance(project).findFile(file) as InlineSqlFile
    val sqlErrors = buildList {
        val annotator = SqlAnnotationHolder { element, s -> add("$s at $element") }

        fun PsiElement.annotateRecursively(annotator: SqlAnnotationHolder) {
            if (this is SqlAnnotatedElement) {
                annotate(annotator)
            }
            children.forEach {
                it.annotateRecursively(annotator)
            }
        }
        PsiTreeUtil.findChildOfType(sqlFile, PsiErrorElement::class.java)?.let { error ->
            annotator.createErrorAnnotation(error, error.errorDescription)
        }
        sqlFile.annotateRecursively(annotator)
    }
    if (sqlErrors.isNotEmpty()) {
        println("Possible SQL errors: $sqlErrors")
    }
    return sqlFile.sqlStmtList?.stmtList ?: emptyList()
}

private fun PsiElement.asSequence(): Sequence<PsiElement> = sequence {
    yield(this@asSequence)
    for (child: PsiElement in children) {
        yieldAll(child.asSequence())
    }
}

private fun CobolBooleanExpr.toFir(dataTree: DataTree): Expression.BooleanExpression {
    val or = booleanExprOr
    val and = booleanExprAnd
    val clause = booleanExprClause
    return when {
        or != null -> Expression.BooleanExpression.Or(
            or.booleanExprClause.toFir(dataTree), or.booleanExpr.toFir(dataTree)
        )

        and != null -> Expression.BooleanExpression.And(
            and.booleanExprClause.toFir(dataTree), and.booleanExpr.toFir(dataTree)
        )

        clause != null -> clause.toFir(dataTree)
        else -> notPossible()
    }
}

private fun CobolBooleanExprClause.toFir(dataTree: DataTree): Expression.BooleanExpression {
    val left = booleanExprClauseLeft.expr.toExpr(dataTree).single()
    val right = booleanExprClauseRight.expr.toExpr(dataTree).single()

    val nt = booleanExprClauseNt
    val bigger = booleanExprClauseBigger
    val smaller = booleanExprClauseSmaller
    return when {
        nt != null -> {
            val equal = Expression.BooleanExpression.Equals(
                left = left, right = right
            )
            if (nt.nt != null) {
                Expression.BooleanExpression.Not(equal)
            } else equal
        }

        bigger != null -> Expression.BooleanExpression.Greater(
            left = left as Expression.NumberExpression,
            right = right as Expression.NumberExpression,
            equals = bigger.eql != null
        )

        smaller != null -> Expression.BooleanExpression.Smaller(
            left = left as Expression.NumberExpression,
            right = right as Expression.NumberExpression,
            equals = smaller.eql != null
        )

        else -> notPossible()
    }
}

private fun CobolExpr.toExpr(dataTree: DataTree): List<Expression> {
    val literal = literal
    val variable = variable
    val stringConcat = stringConcat
    return when {
        literal != null -> when {
            literal.string != null -> listOf(literal.string!!.singleAsString(dataTree))
            literal.number != null -> {
                val number = literal.number!!
                val elementType = number.elementType
                when {
                    elementType == CobolTypes.NUMBER -> listOf(Expression.NumberExpression.NumberLiteral(number.text.toDouble()))
                    number is CobolVariable -> listOf(
                        Expression.NumberExpression.NumberVariable(
                            dataTree.find(number) as NumberElementar
                        )
                    )

                    else -> TODO()
                }
            }

            else -> when (literal.text) {
                "ZERO", "ZEROS", "ZEROES" -> listOf(Expression.NumberExpression.NumberLiteral(0.0))
                else -> TODO("${literal.elementType} ${literal.text}")
            }
        }

        variable != null -> {
            when (val found = dataTree.find(variable)) {
                is Record -> found.elements.map { it.toVariable() }
                is Elementar -> listOf(found.toVariable())
                is WorkingStorage.Sql -> notPossible()
            }
        }

        stringConcat != null -> listOf(stringConcat.toExpr(dataTree))
        else -> TODO("$elementType")
    }
}

private fun PsiElement.singleAsString(dataTree: DataTree): Expression.StringExpression {
    return when {
        elementType == CobolTypes.STRING || elementType == CobolTypes.STRING_VAR -> Expression.StringExpression.StringLiteral(
            value = text.drop(1).dropLast(1)
        )

        this is CobolVariable -> {
            when (val elementar = dataTree.find(this)) {
                is StringElementar -> Expression.StringExpression.StringVariable(elementar)
                is EmptyElementar -> notPossible()
                is NumberElementar -> Expression.StringExpression.Interpolation(
                    Expression.NumberExpression.NumberVariable(
                        elementar
                    )
                )

                is Record -> notPossible()
                is WorkingStorage.Sql -> notPossible()
                is Pointer -> TODO()
            }
        }

        else -> TODO("$elementType")
    }
}

private fun CobolStringConcat.toExpr(dataTree: DataTree): Expression.StringExpression {
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

private inline fun <T, R> Iterable<T>.foldSecond(initial: R, operation: (acc: R, T) -> R?): R {
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

private fun List<WorkingStorage>.find(
    name: String, of: String?
): WorkingStorage? {
    for (record in this) {
        when (record) {
            is Record -> {
                if (of == null && record.name == name) {
                    return record
                }
                if ((of != null && record.name == of) || of == null) {
                    for (elementar in record.elements) {
                        if (elementar.name == name) {
                            return elementar
                        }
                    }
                }
            }

            is WorkingStorage.Sql -> Unit

            is Elementar -> {
                if (of == null && record.name == name) {
                    return record
                }
            }
        }
    }
    return null
}

private fun DataTree.find(variable: CobolVariable): WorkingStorage {
    val name: String = variable.varName.text
    val of = variable.ofClause?.recordID?.varName?.text

    return workingStorage.find(name, of) ?: fileSection.flatMap { it.records }.find(name, of) ?: linkingSection.find(
        name,
        of
    ) ?: error("Elementar $name not found")
}

private fun Elementar.toVariable(): Expression.Variable = when (this) {
    is StringElementar -> Expression.StringExpression.StringVariable(
        target = this
    )

    is NumberElementar -> Expression.NumberExpression.NumberVariable(this)
    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}

public fun notPossible(): Nothing = error("Should not be possible!")
