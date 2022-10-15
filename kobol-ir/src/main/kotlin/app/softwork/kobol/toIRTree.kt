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

fun File.toIR() = toTree().toIRTree()

fun Set<File>.toIR() = toTree().map {
    it.toIRTree()
}

fun CobolFIRTree.toIRTree(): KobolIRTree {
    val dataTypes = data?.workingStorage?.map { it.toIR() } ?: emptyList()
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
    val externalIR = external.toIR(dataTypes)

    val (main, functions) = procedure.functions(dataTypes + externalIR)
    return KobolIRTree(
        name = id.programID.toKotlinName(), main = main, types = functions + dataTypes + externalIR
    )
}

private fun List<Call>.toIR(types: List<Types.Type>): List<Class> = buildMap<String, Class> {
    for (call in this@toIR) {
        val name = call.name
        val old = this[name]
        val newFunction = call.toIrFunctionDeclaration(types)
        if (old == null) {
            this[name] = Class(
                name = call.name,
                constructor = Class.Constructor(emptyList()),
                members = emptyList(),
                functions = listOf(newFunction),
                doc = emptyList(),
                init = listOf(LoadExternal(call.name.lowercase())),
                isObject = true
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
        name = "expr", className = null, value = null, mutable = false, private = false, comments = emptyList()
    )

    is DoubleExpression -> DoubleDeclaration(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
        }, className = when (this) {
            is Literal -> null
            is Variable -> target.className
        }, value = null, mutable = false, private = false, comments = emptyList()
    )

    is IntExpression -> IntDeclaration(
        name = when (this) {
            is Literal -> "expr"
            is Variable -> target.name
        }, className = when (this) {
            is Literal -> null
            is Variable -> target.className
        }, value = null, mutable = false, private = false, comments = emptyList()
    )

    is StringExpression -> StringDeclaration(
        name = when (this) {
            is Variable -> target.name
            else -> "expr"
        }, className = when (this) {
            is Variable -> target.className
            else -> null
        }, value = null, mutable = false, private = false, comments = emptyList()
    )

    is Types.Function.Statement -> TODO()
}

internal fun String.toKotlinName(): String = lowercase().replace("-", "_")

private fun CobolFIRTree.ProcedureTree.functions(
    types: List<Types.Type>
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

    val topLevelStatements = topLevel.mapNotNull { it.toIR(types, sections) }

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
            body = it.statements.mapNotNull { it.toIR(types, sections) },
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

fun CobolFIRTree.ProcedureTree.Expression.NumberExpression.toIR(types: List<Types.Type>): Expression.NumberExpression =
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
            val declaration = types.declaration(target)

            when (target.formatter.numberType) {
                Formatter.NumberType.Int -> IntExpression.IntVariable(
                    declaration as IntDeclaration
                )

                Formatter.NumberType.Double -> DoubleExpression.DoubleVariable(
                    declaration as DoubleDeclaration
                )
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

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> StringExpression.StringVariable(
        types.declaration(target) as StringDeclaration
    )

    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Interpolation -> StringExpression.Interpolation(
        value.toIR(types)
    )
}

private fun List<Types.Type>.declaration(target: CobolFIRTree.DataTree.WorkingStorage.Elementar) = mapNotNull { type ->
    when (type) {
        is GlobalVariable -> if (type.declaration.name == target.name) {
            type.declaration
        } else null

        is Class -> if (type.name == target.recordName) type.members.singleOrNull { it.name == target.name } else null
        else -> null
    }
}.single()


fun CobolFIRTree.ProcedureTree.Statement.toIR(
    types: List<Types.Type>, sections: List<Types.Function>
): Types.Function.Statement? = when (this) {
    is Move -> Assignment(
        declaration = types.declaration(target), newValue = value.toIR(types), comments = comments
    )

    is Display -> Print(expr = expr.toIR(types), comments = comments)

    is Perform -> {
        val functionCall = FunctionCall(
            function = sections.single { section -> sectionName == section.name },
            parameters = emptyList(),
            comments = comments
        )
        val until = until
        if (until == null) {
            functionCall
        } else {
            DoWhile(
                functionCall = functionCall, condition = BooleanExpression.Not(until.toIR(types)), comments = comments
            )
        }
    }

    is ForEach -> Types.Function.Statement.ForEach(
        counter = types.declaration(target = variable) as Declaration.NumberDeclaration,
        from = from.toIR(types),
        step = by?.toIR(types),
        statements = statements.mapNotNull { it.toIR(types, sections) },
        condition = BooleanExpression.Not(until.toIR(types)),
        comments = comments
    )

    is CobolFIRTree.ProcedureTree.Statement.While -> Types.Function.Statement.While(
        condition = BooleanExpression.Not(until.toIR(types)),
        statements = statements.mapNotNull { it.toIR(types, sections) },
        comments = comments
    )

    is Continue -> null
    is GoBack -> Exit(comments)
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
        FunctionCall(
            function = function, parameters = parameters.map {
                it.toIR(types).inferDeclaration()
            }, comments = comments
        )
    }

    is If -> {
        Types.Function.Statement.If(
            condition = condition.toIR(types),
            statements = statements.mapNotNull { it.toIR(types, sections) },
            elseStatements = elseStatements.mapNotNull { it.toIR(types, sections) },
            comments = comments
        )
    }

    is Eval -> {
        val elseCase = other?.let {
            When.Else(
                action = it.action.mapNotNull { it.toIR(types, sections) }, comments = it.comments
            )
        }
        when (values.size) {
            1 -> When.Single(
                expr = values.single().toIR(types), cases = conditions.map {
                    When.Single.Case(
                        condition = it.conditions.single().toIR(types),
                        action = it.action.mapNotNull { it.toIR(types, sections) },
                        comments = it.comments
                    )
                }, elseCase = elseCase, comments = comments
            )

            else -> When.Multiple(
                cases = conditions.map {
                    When.Multiple.Case(condition = it.conditions.map { it.toIR(types) }
                        .mapIndexed<Expression, BooleanExpression> { index, expr ->
                            BooleanExpression.Eq(
                                values[index].toIR(types), expr
                            )
                        }.reduce { left, right ->
                            BooleanExpression.And(left, right)
                        }, action = it.action.mapNotNull { it.toIR(types, sections) }, comments = it.comments
                    )
                }, elseCase = elseCase, comments = comments
            )
        }
    }
}

private fun CobolFIRTree.DataTree.WorkingStorage.Elementar.declaration(className: String? = null) = when (this) {
    is StringElementar -> StringDeclaration(
        name = name, value = value?.let {
            StringExpression.StringLiteral(it)
        }, mutable = true, private = false, comments = comments, className = className
    )

    is NumberElementar -> {
        when (formatter.numberType) {
            Formatter.NumberType.Int -> IntDeclaration(
                name = name, value = value?.let {
                    IntExpression.IntLiteral(it.toInt())
                }, mutable = true, private = false, comments = comments, className = className
            )

            Formatter.NumberType.Double -> DoubleDeclaration(
                name = name, value = value?.let {
                    DoubleExpression.DoubleLiteral(it)
                }, mutable = true, private = false, comments = comments, className = className
            )
        }
    }

    is Pointer -> TODO()
    is EmptyElementar -> TODO()
}

fun CobolFIRTree.DataTree.WorkingStorage.toIR(): Types.Type = when (this) {
    is CobolFIRTree.DataTree.WorkingStorage.Elementar -> {
        when (this) {
            is StringElementar, is NumberElementar -> GlobalVariable(
                declaration = declaration(), const = false, doc = comments
            )

            is Pointer -> TODO()
            is EmptyElementar -> TODO()
        }
    }

    is CobolFIRTree.DataTree.WorkingStorage.Record -> {
        Class(
            name = name, constructor = Class.Constructor(emptyList()), members = elements.map {
                it.declaration(className = name)
            }, functions = emptyList(), doc = comments, init = emptyList(), isObject = true
        )
    }
}
