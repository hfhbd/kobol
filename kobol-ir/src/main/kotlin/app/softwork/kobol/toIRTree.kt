package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.ForEach
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.If
import app.softwork.kobol.KobolIRTree.*
import app.softwork.kobol.KobolIRTree.Expression.*
import app.softwork.kobol.KobolIRTree.Expression.NumberExpression.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.KobolIRTree.Types.Type.*
import java.io.*

fun File.toIR(
    firPlugins: List<FirPlugin> = emptyList(),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null
) = toTree(firPlugins).toIRTree(fileConverter, serialization, sqlPrecompiler)

fun Iterable<File>.toIR(
    firPlugins: List<FirPlugin> = emptyList(),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null
) = toTree(firPlugins).map {
    it.toIRTree(fileConverter, serialization, sqlPrecompiler)
}

fun CobolFIRTree.toIRTree(
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null
): KobolIRTree {
    val name = id.programID.toKotlinName()
    val fileHandler = fileConverter?.invoke(name)
    val serialization = serialization?.invoke(name)
    val sqlCompiler = sqlPrecompiler?.invoke(name)

    val dataTypes = mutableListOf<Types.Type>()

    val fileSection = data?.fileSection?.takeIf { it.isNotEmpty() }
    if (fileSection != null) {
        requireNotNull(serialization)
        for (file in fileSection) {
            val converted = serialization.fileSection(file)
            dataTypes.addAll(converted)
        }
    }

    val sqlInit = mutableListOf<Types.Function.Statement>()
    val workingStorage = data?.workingStorage
    if (workingStorage != null) {
        for (data in workingStorage) {
            when (val ir = data.toIR(name, sqlCompiler)) {
                is IRResult.Typ -> dataTypes.add(ir.type)
                is IRResult.SqlInit -> sqlInit.addAll(ir.sqlInit)
            }
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
        dataTypes + externalIR, sqlInit, fileHandler, serialization, sqlCompiler
    )
    sqlCompiler?.close()
    serialization?.close()
    fileHandler?.close()
    return KobolIRTree(
        name = name, main = main, types = functions + dataTypes + externalIR
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
        name = name, parameters = parameters.map {
            it.toIR(types = types).inferDeclaration()
        }, returnType = Void, private = false, external = true, doc = listOf(), body = emptyList()
    )
}

private fun Expression.inferDeclaration(): Declaration = when (this) {
    is BooleanExpression -> BooleanDeclaration(
        name = "expr", value = null, mutable = false, private = false, comments = emptyList(), const = true, length = -1
    )

    is DoubleExpression -> DoubleDeclaration(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
            is DoubleExpression.DoubleVariable.Use -> variable.target.name
        }, value = null, mutable = false, private = false, comments = emptyList(), const = true, length = -1
    )

    is IntExpression -> IntDeclaration(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
            is IntExpression.IntVariable.Use -> variable.target.name
        }, value = null, mutable = false, private = false, comments = emptyList(), const = true, length = -1
    )

    is StringExpression -> StringDeclaration(
        name = when (this) {
            is Variable -> target.name
            is StringExpression.StringVariable.Use -> variable.target.name
            is StringExpression.Interpolation, is StringExpression.Concat, is Literal -> "expr"
        }, value = null, mutable = false, private = false, comments = emptyList(), const = true, length = -1
    )

    is Types.Function.Statement -> TODO()
    is GlobalVariable -> declaration
    is ObjectVariable -> target
}

internal fun String.toKotlinName(): String = lowercase().replace("-", "_")

private fun CobolFIRTree.ProcedureTree.functions(
    types: List<Types.Type>,
    initSql: List<Types.Function.Statement>,
    fileHandler: FileHandling?,
    serialization: SerializationPlugin?,
    sqlPrecompiler: SqlPrecompiler?
): Pair<Types.Function, List<Types.Function>> {
    val sections = sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Void,
            body = emptyList(),
            private = false,
            doc = it.comments
        )
    }

    val topLevelStatements =
        initSql + topLevel.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) }

    val main = Types.Function(
        name = "main", parameters = emptyList(), returnType = Void, body = topLevelStatements + sections.map {
            FunctionCall(
                it, parameters = emptyList(), comments = emptyList()
            )
        }, private = false, doc = comments
    )
    val sectionsWithResolvedCalls = this@functions.sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Void,
            body = it.statements.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) },
            private = false,
            doc = it.comments
        )
    }

    return main to sectionsWithResolvedCalls
}

fun CobolFIRTree.ProcedureTree.Expression.toIR(types: List<Types.Type>): Expression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression -> toIR(types)
    is CobolFIRTree.ProcedureTree.Expression.NumberExpression -> toIR(types)
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression -> toIR(types)
}

fun CobolFIRTree.ProcedureTree.Expression.BooleanExpression.toIR(types: List<Types.Type>): BooleanExpression =
    when (this) {
        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.And -> BooleanExpression.And(
            left = left.toIR(types), right = right.toIR(types)
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Equals -> BooleanExpression.Eq(
            left = left.toIR(types), right = right.toIR(types)
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Greater -> BooleanExpression.Bigger(
            left = left.toIR(
                types
            ), right = right.toIR(types), equals = equals
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Not -> BooleanExpression.Not(
            condition = target.toIR(types)
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Or -> BooleanExpression.Or(
            left = left.toIR(types), right = right.toIR(types)
        )

        is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Smaller -> BooleanExpression.Smaller(
            left = left.toIR(types), right = right.toIR(types), equals = equals
        )
    }

fun CobolFIRTree.ProcedureTree.Expression.NumberExpression.toIR(types: List<Types.Type>): NumberExpression =
    when (this) {
        is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral -> {
            val isInt = value.isInt()
            if (isInt != null) {
                IntExpression.IntLiteral(isInt)
            } else {
                DoubleExpression.DoubleLiteral(value)
            }
        }

        is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable -> {
            when (val declaration = types.declaration(target)) {
                is Declaration -> {
                    when (target.formatter.numberType) {
                        Formatter.NumberType.Int -> IntExpression.IntVariable(
                            declaration as IntDeclaration
                        )

                        Formatter.NumberType.Double -> DoubleExpression.DoubleVariable(
                            declaration as DoubleDeclaration
                        )
                    }
                }

                is IntExpression.IntVariable.Use -> {
                    declaration
                }

                is DoubleExpression.DoubleVariable.Use -> {
                    declaration
                }

                else -> TODO()
            }
        }
    }

fun Double.isInt(): Int? = when {
    this > Int.MAX_VALUE -> null
    this < Int.MIN_VALUE -> null
    rem(1) == 0.0 -> toInt()
    else -> null
}

fun CobolFIRTree.ProcedureTree.Expression.StringExpression.toIR(
    types: List<Types.Type>
): StringExpression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> StringExpression.StringLiteral(
        value = value
    )

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> StringExpression.Concat(
        left = left.toIR(types), right = right.toIR(types)
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
        value.toIR(types)
    )
}

private fun List<Types.Type>.declaration(target: CobolFIRTree.DataTree.WorkingStorage.Elementar) = mapNotNull { type ->
    when (type) {
        is GlobalVariable -> if (type.declaration.name == target.name) {
            type.declaration
        } else null

        is Class -> if (type.name == target.recordName) {
            val member = type.members.singleOrNull { it.name == target.name } ?: return@mapNotNull null
            when (member) {
                is ObjectDeclaration -> TODO()
                is BooleanDeclaration -> TODO()
                is DoubleDeclaration -> TODO()
                is IntDeclaration -> IntExpression.IntVariable.Use(
                    type.variable(), member.variable() as IntExpression.IntVariable, emptyList()
                )

                is StringDeclaration -> StringExpression.StringVariable.Use(
                    type.variable(), member.variable() as StringExpression.StringVariable, emptyList()
                )
            }

        } else null

        else -> null
    }
}.single()

fun CobolFIRTree.ProcedureTree.Statement.toIR(
    types: List<Types.Type>,
    sections: List<Types.Function>,
    fileHandler: FileHandling?,
    serialization: SerializationPlugin?,
    sqlPrecompiler: SqlPrecompiler?
): List<Types.Function.Statement> = when (this) {
    is Move -> listOf(
        Assignment(
            declaration = types.declaration(target), newValue = value.toIR(types), comments = comments
        )
    )

    is Display -> listOf(Print(expr = expr.toIR(types), comments = comments))

    is Perform -> {
        val functionCall = FunctionCall(
            function = sections.single { section -> sectionName == section.name },
            parameters = emptyList(),
            comments = comments
        )
        val until = until
        if (until == null) {
            listOf(functionCall)
        } else {
            listOf(
                DoWhile(
                    functionCall = functionCall,
                    condition = BooleanExpression.Not(until.toIR(types)),
                    comments = comments
                )
            )
        }
    }

    is ForEach -> listOf(
        For(
            counter = types.declaration(target = variable) as NumberDeclaration,
            from = from.toIR(types),
            step = by?.toIR(types),
            statements = statements.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) },
            condition = BooleanExpression.Not(until.toIR(types)),
            comments = comments
        )
    )

    is CobolFIRTree.ProcedureTree.Statement.While -> listOf(
        Types.Function.Statement.While(
            condition = BooleanExpression.Not(until.toIR(types)),
            statements = statements.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) },
            comments = comments
        )
    )

    is Continue -> emptyList()
    is GoBack -> listOf(Exit(comments))
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
                function = function, parameters = parameters.map {
                    it.toIR(types)
                }, comments = comments
            )
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
            statements = statements.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) },
            elseStatements = elseStatements.flatMap {
                it.toIR(
                    types, sections, fileHandler, serialization, sqlPrecompiler
                )
            },
            comments = comments
        )
    )

    is Eval -> {
        val elseCase = other?.let {
            When.Else(
                action = it.action.flatMap { it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler) },
                comments = it.comments
            )
        }
        when (values.size) {
            1 -> listOf(
                When.Single(
                    expr = values.single().toIR(types), cases = conditions.map {
                        When.Single.Case(
                            condition = it.conditions.single().toIR(types), action = it.action.flatMap {
                                it.toIR(
                                    types, sections, fileHandler, serialization, sqlPrecompiler
                                )
                            }, comments = it.comments
                        )
                    }, elseCase = elseCase, comments = comments
                )
            )

            else -> listOf(
                When.Multiple(
                    cases = conditions.map {
                        When.Multiple.Case(
                            condition = it.conditions.map { it.toIR(types) }
                                .mapIndexed<Expression, BooleanExpression> { index, expr ->
                                    BooleanExpression.Eq(
                                        values[index].toIR(types), expr
                                    )
                                }.reduce { left, right ->
                                    BooleanExpression.And(left, right)
                                }, action = it.action.flatMap {
                                it.toIR(
                                    types, sections, fileHandler, serialization, sqlPrecompiler
                                )
                            }, comments = it.comments

                        )
                    }, elseCase = elseCase, comments = comments
                )
            )
        }
    }

    is Read -> {
        requireNotNull(serialization).readSequence(this) {
            flatMap {
                it.toIR(types, sections, fileHandler, serialization, sqlPrecompiler)
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
        length = formatter.length()
    )

    is NumberElementar -> {
        when (formatter.numberType) {
            Formatter.NumberType.Int -> IntDeclaration(
                name = name,
                value = value?.let { IntExpression.IntLiteral(it.toInt()) },
                mutable = true,
                private = false,
                comments = comments,
                const = false,
                length = formatter.length()
            )

            Formatter.NumberType.Double -> DoubleDeclaration(
                name = name,
                value = value?.let { DoubleExpression.DoubleLiteral(it) },
                mutable = true,
                private = false,
                comments = comments,
                const = false,
                length = formatter.length()
            )
        }
    }

    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}

private sealed interface IRResult {
    data class Typ(val type: Types.Type) : IRResult
    data class SqlInit(val sqlInit: List<Types.Function.Statement>) : IRResult
}

private fun CobolFIRTree.DataTree.WorkingStorage.toIR(packageName: String, sqlPrecompiler: SqlPrecompiler?): IRResult =
    when (this) {
        is CobolFIRTree.DataTree.WorkingStorage.Elementar -> IRResult.Typ(
            when (this) {
                is StringElementar, is NumberElementar -> GlobalVariable(
                    declaration = declaration(), doc = comments
                )

                is Pointer -> TODO()
                is EmptyElementar -> TODO()
            }
        )

        is CobolFIRTree.DataTree.WorkingStorage.Sql -> {
            requireNotNull(sqlPrecompiler)
            IRResult.SqlInit(sqlPrecompiler.convert(this))
        }

        is CobolFIRTree.DataTree.WorkingStorage.Record -> IRResult.Typ(toIR(packageName))
    }

fun CobolFIRTree.DataTree.WorkingStorage.Record.toIR(packageName: String) = Class(
    name = name,
    packageName = packageName,
    constructor = emptyList(),
    members = elements.map {
        it.declaration()
    },
    functions = emptyList(), doc = comments, init = emptyList(), isObject = true,
)
