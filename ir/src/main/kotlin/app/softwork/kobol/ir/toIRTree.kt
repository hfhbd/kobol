package app.softwork.kobol.ir

import app.softwork.kobol.fir.CobolFIRTree
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.ForEach
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.If
import app.softwork.kobol.fir.FirPlugin
import app.softwork.kobol.fir.toTree
import app.softwork.kobol.ir.KobolIRTree.Expression
import app.softwork.kobol.ir.KobolIRTree.Expression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.DoubleExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable
import app.softwork.kobol.ir.KobolIRTree.Types
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import java.io.File
import java.nio.file.Path

public fun File.toIR(
    firPlugins: List<FirPlugin> = emptyList(),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
    irPlugins: List<IrPlugin> = emptyList(),
    procedureName: ProcedureName? = null,
): KobolIRTree = setOf(this).toIR(
    parentFile!!.toPath(),
    firPlugins,
    fileConverter,
    serialization,
    sqlPrecompiler,
    controlFlowHandling,
    irPlugins,
    procedureName,
).single()

public fun Iterable<File>.toIR(
    absoluteBasePath: Path,
    firPlugins: Iterable<FirPlugin> = emptyList(),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
    irPlugins: Iterable<IrPlugin> = emptyList(),
    procedureName: ProcedureName? = null,
): Iterable<KobolIRTree> {
    val firTrees = toTree(absoluteBasePath, firPlugins)

    var irTrees = firTrees.map {
        it.toIRTree(fileConverter, serialization, sqlPrecompiler, controlFlowHandling, procedureName)
    }.associateBy { it.id }

    for (irPlugin in irPlugins) {
        for (irTree in irTrees) {
            val other = irTrees - irTree.key
            val newTrees = irPlugin(irTree.value, other.values).associateBy { it.id }
            irTrees = newTrees
        }
    }

    return irTrees.values
}

public fun CobolFIRTree.toIRTree(
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
    procedureName: ProcedureName? = null,
): KobolIRTree {
    val name = id.programID.toKotlinName()
    val fileHandler = fileConverter?.invoke(name)
    val serialization = serialization?.invoke(name)
    val sqlCompiler = sqlPrecompiler?.invoke(name)
    val controlFlowHandling = controlFlowHandling?.invoke(name)

    val dataTypes = mutableListOf<Types.Type>()

    val fileSection = data.fileSection.takeIf { it.isNotEmpty() }
    if (fileSection != null) {
        requireNotNull(serialization)
        for (file in fileSection) {
            val converted = serialization.fileSection(file)
            dataTypes.addAll(converted)
        }
    }

    val sqlInit = data.sql.flatMap {
        requireNotNull(sqlCompiler)
        sqlCompiler.convert(it)
    }

    for (data in data.workingStorage) {
        val ir = data.toIR(name)
        dataTypes.add(ir)
    }

    val linkingTypes = data.linkingSection.map { link ->
        val result = link.toIR(name)
        result
    }

    val linkingElementars = mutableListOf<GlobalVariable>()

    for (link in linkingTypes) {
        when (link) {
            is Class -> dataTypes.add(link)
            is GlobalVariable -> linkingElementars.add(link)
            is Natives -> Unit
        }
    }

    val external = procedure.topLevel.mapNotNull {
        when (it) {
            is Call -> it
            else -> null
        }
    } + procedure.sections.flatMap {
        it.statements.mapNotNull {
            when (it) {
                is Call -> it
                else -> null
            }
        }
    }
    val externalIR = external.toIR(dataTypes, name)

    val (main, functions) = procedure.functions(
        mainName = procedureName?.name(fileName, id) ?: name,
        linkingTypes = linkingTypes,
        types = dataTypes + externalIR + linkingElementars,
        sqlInit,
        fileHandler,
        serialization,
        sqlCompiler,
        controlFlowHandling,
    )
    sqlCompiler?.close()
    serialization?.close()
    fileHandler?.close()
    controlFlowHandling?.close()
    return KobolIRTree(
        id = fileName,
        name = name,
        packageName = packageName,
        main = main,
        types = functions + dataTypes + externalIR,
    )
}

private fun List<Call>.toIR(types: List<Types.Type>, packageName: String): List<Class> = buildMap<String, Class> {
    for (call in this@toIR) {
        val name = call.name
        val old = this[name]
        val newFunction = call.toIrFunctionDeclaration(types)
        if (old == null) {
            this[name] = Class(
                name = call.name,
                packageName = packageName,
                constructor = emptyList(),
                members = emptyList(),
                functions = listOf(newFunction),
                doc = emptyList(),
                init = listOf(LoadExternal(call.name.lowercase())),
                isObject = true,
            )
        } else if (newFunction !in old.functions) {
            this[name] = old.copy(functions = old.functions + newFunction)
        }
    }
}.values.toList()

private fun Call.toIrFunctionDeclaration(types: List<Types.Type>): Types.Function {
    return Types.Function(
        name = name,
        parameters = parameters.map {
            it.toIR(types = types).inferDeclaration()
        },
        returnType = Natives.Void,
        private = false,
        external = true,
        doc = listOf(),
        body = mutableListOf(),
    )
}

private fun Expression.inferDeclaration(): Declaration = when (this) {
    is BooleanExpression -> BooleanDeclaration(
        name = "expr",
        value = null,
        mutable = false,
        private = false,
        comments = emptyList(),
        const = true,
        length = -1,
    )

    is DoubleExpression -> DoubleDeclaration(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
            is DoubleExpression.DoubleVariable.Use -> variable.target.name
        },
        value = null,
        mutable = false,
        private = false,
        comments = emptyList(),
        const = true,
        length = -1,
        isSigned = false,
    )

    is IntExpression -> IntDeclaration.Normal(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
            is IntVariable.Use -> variable.target.name
        },
        value = null,
        mutable = false,
        private = false,
        comments = emptyList(),
        const = true,
        length = -1,
        isSigned = false,
    )

    is StringExpression -> StringDeclaration(
        name = when (this) {
            is Variable -> target.name
            is StringExpression.StringVariable.Use -> variable.target.name
            is StringExpression.Interpolation, is StringExpression.Concat, is Literal -> "expr"
        },
        value = null,
        mutable = false,
        private = false,
        comments = emptyList(),
        const = true,
        length = -1,
    )

    is Types.Function.Statement -> TODO()
    is GlobalVariable -> declaration
    is ObjectVariable -> target
}

public fun String.toKotlinName(): String = lowercase().replace("-", "_")

private fun CobolFIRTree.ProcedureTree.functions(
    mainName: String,
    linkingTypes: List<Types.Type>,
    types: List<Types.Type>,
    initSql: List<Types.Function.Statement>,
    fileHandler: FileHandling?,
    serialization: SerializationPlugin?,
    sqlPrecompiler: SqlPrecompiler?,
    controlFlowHandling: ControlFlowHandling?,
): Pair<Types.Function, List<Types.Function>> {
    val sections = sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Natives.Void,
            body = mutableListOf(),
            private = false,
            doc = it.comments,
        )
    }

    val topLevelStatements =
        initSql + topLevel.flatMap {
            it.toIR(
                types,
                sections,
                fileHandler,
                serialization,
                sqlPrecompiler,
                controlFlowHandling,
            )
        }

    val linkingParameters = linkingTypes.mapNotNull {
        when (it) {
            is Class -> ObjectDeclaration(
                name = it.name,
                type = it,
                value = null,
                nullable = false,
            )

            is GlobalVariable -> it.declaration
            is Natives -> null
        }
    }

    val main = Types.Function(
        name = mainName,
        parameters = linkingParameters,
        returnType = Natives.Void,
        body = (
            topLevelStatements + sections.map {
                FunctionCall(
                    it,
                    parameters = emptyList(),
                    comments = emptyList(),
                )
            }
            ).toMutableList(),
        private = false,
        doc = comments,
        isEntryPoint = true,
    )

    val sectionsWithResolvedCalls = this@functions.sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Natives.Void,
            body = it.statements.flatMap {
                it.toIR(
                    types,
                    sections,
                    fileHandler,
                    serialization,
                    sqlPrecompiler,
                    controlFlowHandling,
                )
            }.toMutableList(),
            private = false,
            doc = it.comments,
        )
    }

    require(mainName !in sectionsWithResolvedCalls.map { it.name }) {
        "Duplicate function names found: Either rename main procedure $mainName using ProcedureName " +
            "or rename a section and its caller."
    }

    return main to sectionsWithResolvedCalls
}

private fun CobolFIRTree.ProcedureTree.Expression.toIR(types: List<Types.Type>): Expression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression -> toIR(types)
    is CobolFIRTree.ProcedureTree.Expression.NumberExpression -> toIR(types)
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression -> toIR(types)
}

private fun CobolFIRTree.ProcedureTree.Expression.BooleanExpression.toIR(types: List<Types.Type>): BooleanExpression =
    when (this) {
        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.And -> BooleanExpression.And(
            left = left.toIR(types),
            right = right.toIR(types),
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Equals -> BooleanExpression.Eq(
            left = left.toIR(types),
            right = right.toIR(types),
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Greater -> BooleanExpression.Bigger(
            left = left.toIR(
                types,
            ),
            right = right.toIR(types),
            equals = equals,
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Not -> BooleanExpression.Not(
            condition = target.toIR(types),
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Or -> BooleanExpression.Or(
            left = left.toIR(types),
            right = right.toIR(types),
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Smaller -> BooleanExpression.Smaller(
            left = left.toIR(types),
            right = right.toIR(types),
            equals = equals,
        )
    }

private fun CobolFIRTree.ProcedureTree.Expression.NumberExpression.toIR(types: List<Types.Type>): NumberExpression =
    when (this) {
        is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral -> {
            val isInt = value.isInt()
            if (isInt != null) {
                IntLiteral(isInt)
            } else {
                DoubleExpression.DoubleLiteral(value)
            }
        }

        is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable -> {
            when (val declaration = types.declaration(target)) {
                is Declaration -> {
                    when (target.formatter.numberType) {
                        Formatter.NumberType.Int -> IntVariable(
                            declaration as IntDeclaration,
                        )

                        Formatter.NumberType.Double -> DoubleExpression.DoubleVariable(
                            declaration as DoubleDeclaration,
                        )
                    }
                }

                is IntVariable.Use -> {
                    declaration
                }

                is DoubleExpression.DoubleVariable.Use -> {
                    declaration
                }

                else -> TODO()
            }
        }
    }

private fun Double.isInt(): Int? = when {
    this > Int.MAX_VALUE -> null
    this < Int.MIN_VALUE -> null
    rem(1) == 0.0 -> toInt()
    else -> null
}

private fun CobolFIRTree.ProcedureTree.Expression.StringExpression.toIR(
    types: List<Types.Type>,
): StringExpression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> StringExpression.StringLiteral(
        value = value,
    )

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> StringExpression.Concat(
        left = left.toIR(types),
        right = right.toIR(types),
    )

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> {
        when (val dec = types.declaration(target)) {
            is StringDeclaration -> {
                StringExpression.StringVariable(dec)
            }

            is StringExpression.StringVariable.Use -> dec
            else -> StringExpression.Interpolation(dec as Expression)
        }
    }

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Interpolation -> StringExpression.Interpolation(
        value.toIR(types),
    )
}

private fun List<Types.Type>.declaration(target: CobolFIRTree.DataTree.WorkingStorage.Elementar) = mapNotNull { type ->
    when (type) {
        is GlobalVariable -> if (type.declaration.name == target.name) {
            type.declaration
        } else {
            null
        }

        is Class -> if (type.name == target.recordName) {
            val member = type.members.singleOrNull { it.name == target.name } ?: return@mapNotNull null
            when (member) {
                is ObjectDeclaration -> TODO()
                is BooleanDeclaration -> TODO()
                is DoubleDeclaration -> TODO()
                is Declaration.Array -> TODO()
                is IntDeclaration -> IntVariable.Use(
                    type.variable(),
                    member.variable() as IntVariable,
                    emptyList(),
                )

                is StringDeclaration -> StringExpression.StringVariable.Use(
                    type.variable(),
                    member.variable() as StringExpression.StringVariable,
                    emptyList(),
                )
            }
        } else {
            null
        }

        is Natives -> null
    }
}.singleOrNull() ?: error("${target.name} of ${target.recordName} not found in $this")

private fun CobolFIRTree.ProcedureTree.Statement.toIR(
    types: List<Types.Type>,
    sections: List<Types.Function>,
    fileHandler: FileHandling?,
    serialization: SerializationPlugin?,
    sqlPrecompiler: SqlPrecompiler?,
    controlFlowHandling: ControlFlowHandling?,
): List<Types.Function.Statement> = when (this) {
    is Move -> listOf(
        Assignment(
            declaration = types.declaration(target),
            newValue = value.toIR(types),
            comments = comments,
        ),
    )

    is Add -> listOf(
        Math(
            declaration = types.declaration(target),
            value = value.toIR(types),
            op = Math.Operation.Add,
            comments = comments,
        ),
    )

    is Sub -> listOf(
        Math(
            declaration = types.declaration(target),
            value = value.toIR(types),
            op = Math.Operation.Sub,
            comments = comments,
        ),
    )

    is Display -> listOf(Print(expr = expr.toIR(types), comments = comments))

    is Perform -> {
        val until = until
        val functionCall = FunctionCall(
            function = sections.single { section -> sectionName == section.name },
            parameters = emptyList(),
            comments = if (until == null) comments else emptyList(),
        )
        if (until == null) {
            listOf(functionCall)
        } else {
            when (testing) {
                Perform.Testing.After -> listOf(
                    DoWhile(
                        statements = listOf(functionCall),
                        condition = BooleanExpression.Not(until.toIR(types)),
                        comments = comments,
                    ),
                )

                Perform.Testing.Before -> listOf(
                    Types.Function.Statement.While(
                        statements = listOf(functionCall),
                        condition = BooleanExpression.Not(until.toIR(types)),
                        comments = comments,
                    ),
                )
            }
        }
    }

    is ForEach -> listOf(
        For(
            counter = types.declaration(target = variable) as NumberDeclaration,
            from = from.toIR(types),
            step = by?.toIR(types),
            statements = statements.flatMap {
                it.toIR(
                    types,
                    sections,
                    fileHandler,
                    serialization,
                    sqlPrecompiler,
                    controlFlowHandling,
                )
            },
            condition = BooleanExpression.Not(until.toIR(types)),
            comments = comments,
        ),
    )

    is CobolFIRTree.ProcedureTree.Statement.While -> listOf(
        Types.Function.Statement.While(
            condition = BooleanExpression.Not(until.toIR(types)),
            statements = statements.flatMap {
                it.toIR(
                    types,
                    sections,
                    fileHandler,
                    serialization,
                    sqlPrecompiler,
                    controlFlowHandling,
                )
            },
            comments = comments,
        ),
    )

    is Continue -> emptyList()
    is GoBack -> requireNotNull(controlFlowHandling) { "Got GoBack without ControlFlowHandling plugin" }.goBack(this)
    is StopRun -> requireNotNull(controlFlowHandling) { "Got StopRun without ControlFlowHandling plugin" }
        .stopRun(
            this,
            types.mapNotNull {
                val globalVariable = it as? GlobalVariable ?: return@mapNotNull null
                globalVariable.declaration as? IntDeclaration.ReturnCode
            }.single().variable() as IntVariable,
        )

    is Call -> {
        val `class` = types.single {
            when (it) {
                is Class -> it.name == name
                else -> false
            }
        } as Class

        val function = `class`.functions.single {
            it.name == name
        }
        listOf(
            FunctionCall(
                function = function,
                parameters = parameters.map {
                    it.toIR(types)
                },
                comments = comments,
            ),
        )
    }

    is Sql -> requireNotNull(sqlPrecompiler).convert(this, {
        it.toIR(types) as Variable
    }) {
        it.declaration()
    }

    is If -> listOf(
        Types.Function.Statement.If(
            condition = condition.toIR(types),
            statements = statements.flatMap {
                it.toIR(
                    types,
                    sections,
                    fileHandler,
                    serialization,
                    sqlPrecompiler,
                    controlFlowHandling,
                )
            },
            elseStatements = elseStatements.flatMap {
                it.toIR(
                    types,
                    sections,
                    fileHandler,
                    serialization,
                    sqlPrecompiler,
                    controlFlowHandling,
                )
            },
            comments = comments,
        ),
    )

    is Eval -> {
        val elseCase = other?.let {
            When.Else(
                action = it.action.flatMap {
                    it.toIR(
                        types,
                        sections,
                        fileHandler,
                        serialization,
                        sqlPrecompiler,
                        controlFlowHandling,
                    )
                },
                comments = it.comments,
            )
        }
        when (values.size) {
            1 -> listOf(
                When.Single(
                    expr = values.single().toIR(types),
                    cases = conditions.map {
                        When.Single.Case(
                            condition = it.conditions.single().toIR(types),
                            action = it.action.flatMap {
                                it.toIR(
                                    types,
                                    sections,
                                    fileHandler,
                                    serialization,
                                    sqlPrecompiler,
                                    controlFlowHandling,
                                )
                            },
                            comments = it.comments,
                        )
                    },
                    elseCase = elseCase,
                    comments = comments,
                ),
            )

            else -> listOf(
                When.Multiple(
                    cases = conditions.map {
                        When.Multiple.Case(
                            condition = it.conditions.map { it.toIR(types) }
                                .mapIndexed<Expression, BooleanExpression> { index, expr ->
                                    BooleanExpression.Eq(
                                        values[index].toIR(types),
                                        expr,
                                    )
                                }.reduce { left, right ->
                                    BooleanExpression.And(left, right)
                                },
                            action = it.action.flatMap {
                                it.toIR(
                                    types,
                                    sections,
                                    fileHandler,
                                    serialization,
                                    sqlPrecompiler,
                                    controlFlowHandling,
                                )
                            },
                            comments = it.comments,

                        )
                    },
                    elseCase = elseCase,
                    comments = comments,
                ),
            )
        }
    }

    is Read -> {
        requireNotNull(serialization).readSequence(this) {
            flatMap {
                it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler, controlFlowHandling)
            }
        }
    }

    is Write -> requireNotNull(serialization).write(this)

    is Close -> requireNotNull(fileHandler).handleClose(this)
    is Open -> requireNotNull(fileHandler).handleOpen(this)
}

private fun CobolFIRTree.DataTree.WorkingStorage.Elementar.declaration() = when (this) {
    is StringElementar -> StringDeclaration(
        name = name,
        value = value?.let { StringExpression.StringLiteral(it) },
        mutable = true,
        private = false,
        comments = comments,
        const = false,
        synthetic = synthetic,
        length = formatter.length(),
    )

    is NumberElementar -> {
        when (formatter.numberType) {
            Formatter.NumberType.Int -> IntDeclaration.Normal(
                name = name,
                value = value?.let { IntLiteral(it.toInt()) },
                mutable = true,
                private = false,
                comments = comments,
                const = false,
                length = formatter.length(),
                isSigned = signed,
                synthetic = synthetic,
            )

            Formatter.NumberType.Double -> DoubleDeclaration(
                name = name,
                value = value?.let { DoubleExpression.DoubleLiteral(it) },
                mutable = true,
                private = false,
                comments = comments,
                const = false,
                length = formatter.length(),
                isSigned = signed,
                synthetic = synthetic,
            )
        }
    }

    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}

private fun CobolFIRTree.DataTree.WorkingStorage.toIR(packageName: String): Types.Type =
    when (this) {
        is CobolFIRTree.DataTree.WorkingStorage.Elementar ->
            when (this) {
                is NumberElementar.ReturnCode -> GlobalVariable(
                    declaration =
                    IntDeclaration.ReturnCode(
                        name = name,
                        value = IntLiteral(value.toInt()),
                        mutable = true,
                        const = false,
                        length = formatter.length(),
                        isSigned = signed,
                    ),
                    doc = comments,
                )

                is StringElementar, is NumberElementar -> GlobalVariable(
                    declaration = declaration(),
                    doc = comments,
                )

                is Pointer -> TODO()
                is EmptyElementar -> TODO()
            }

        is CobolFIRTree.DataTree.WorkingStorage.Record -> toIR(packageName)
    }

public fun CobolFIRTree.DataTree.WorkingStorage.Record.toIR(packageName: String): Class = Class(
    name = name,
    packageName = packageName,
    constructor = emptyList(),
    members = elements.map {
        it.declaration()
    },
    functions = emptyList(),
    doc = comments,
    init = emptyList(),
    isObject = true,
)
