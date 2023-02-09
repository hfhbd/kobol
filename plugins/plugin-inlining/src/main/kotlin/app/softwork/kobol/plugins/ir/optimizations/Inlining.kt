package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.*
import app.softwork.kobol.ir.KobolIRTree.Expression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*

public class Inlining : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree.inlineGlobalVariables()
    }
}

/**
 * If a global variable is only used once, move it into this function.
 */
private fun KobolIRTree.inlineGlobalVariables(): KobolIRTree {
    val globalVariables = types.mapNotNull {
        when (it) {
            is GlobalVariable -> it
            else -> null
        }
    }

    val removed = globalVariables.mapNotNull { globalVariable ->
        val writes = findWriteUsages(globalVariable.declaration)
        val allWrites = (writes.types + writes.main)
        val onlyWrittenOnce = if (allWrites.size == 1) {
            allWrites.single()
        } else return@mapNotNull null
        val reads = findReadUsages(globalVariable.declaration)
        val onlyReadOnce = (reads.types + reads.main).singleOrNull() ?: return@mapNotNull null
        if (onlyWrittenOnce == null || onlyWrittenOnce == onlyReadOnce) {
            val usage = when (onlyReadOnce) {
                is Types.Function -> onlyReadOnce
                is Class -> onlyReadOnce.functions.single {
                    it.contains(globalVariable.declaration) != null
                }

                else -> error("Unsupported type for inlineGlobalVariables: $onlyWrittenOnce $globalVariable")
            }
            val body = usage.body
            val declaration = globalVariable.declaration.let {
                if (onlyWrittenOnce == null) {
                    when (it) {
                        is Declaration.ObjectDeclaration -> it.copy(
                            mutable = false
                        )

                        is Declaration.BooleanDeclaration -> it.copy(
                            mutable = false
                        )

                        is Declaration.DoubleDeclaration -> it.copy(
                            mutable = false
                        )

                        is Declaration.IntDeclaration -> it.copy(
                            mutable = false
                        )

                        is Declaration.StringDeclaration -> it.copy(
                            mutable = false
                        )
                    }
                } else it
            }
            val variable = declaration.variable()
            body.add(0, declaration) // create local variable
            body.replaceAll {
                it.useInlineVariable(globalVariable, variable)
            }

            globalVariable
        } else null
    }

    return copy(
        types = types.filter {
            it !in removed
        }
    )
}


private fun Expression.useInlineVariable(globalVariable: GlobalVariable, variable: Variable): Expression = when (this) {
    is BooleanExpression.And -> copy(
        left = left.useInlineVariable(globalVariable, variable) as BooleanExpression,
        right = right.useInlineVariable(globalVariable, variable) as BooleanExpression
    )

    is BooleanExpression.Bigger -> copy(
        left = left.useInlineVariable(globalVariable, variable) as NumberExpression,
        right = right.useInlineVariable(globalVariable, variable) as NumberExpression
    )

    is BooleanExpression.BooleanLiteral -> this
    is BooleanExpression.Eq -> copy(
        left = left.useInlineVariable(globalVariable, variable),
        right = right.useInlineVariable(globalVariable, variable)
    )

    is BooleanExpression.Not -> copy(
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
    )

    is BooleanExpression.NotEq -> copy(
        left = left.useInlineVariable(globalVariable, variable),
        right = right.useInlineVariable(globalVariable, variable)
    )

    is BooleanExpression.Or -> copy(
        left = left.useInlineVariable(globalVariable, variable) as BooleanExpression,
        right = right.useInlineVariable(globalVariable, variable) as BooleanExpression
    )

    is BooleanExpression.Smaller -> copy(
        left = left.useInlineVariable(globalVariable, variable) as NumberExpression,
        right = right.useInlineVariable(globalVariable, variable) as NumberExpression
    )

    is FunctionCall -> copy(
        parameters = parameters.useInlineVariable(globalVariable, variable)
    )

    is NumberExpression.DoubleExpression.DoubleLiteral -> this
    is NumberExpression.IntExpression.IntLiteral -> this
    is StringExpression.StringLiteral -> this
    is NumberExpression.DoubleExpression.DoubleVariable -> this
    is NumberExpression.DoubleExpression.DoubleVariable.Use -> this
    is NumberExpression.IntExpression.IntVariable -> this
    is NumberExpression.IntExpression.IntVariable.Use -> this
    is StringExpression.Concat -> copy(
        left = left.useInlineVariable(globalVariable, variable),
        right = right.useInlineVariable(globalVariable, variable)
    )

    is StringExpression.Interpolation -> copy(
        expr = expr.useInlineVariable(globalVariable, variable),
    )

    is StringExpression.StringVariable -> this
    is StringExpression.StringVariable.Use -> this
    is If -> copy(
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
        statements = statements.useInlineVariable(globalVariable, variable),
        elseStatements = elseStatements.useInlineVariable(globalVariable, variable),
        elseIfs = elseIfs.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
                statements = it.statements.useInlineVariable(globalVariable, variable)
            )
        }
    )

    is Use -> copy(
        target = target.useInlineVariable(globalVariable, variable),
        action = action.useInlineVariable(globalVariable, variable)
    )

    is When.Multiple -> copy(
        cases = cases.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
                action = it.action.useInlineVariable(globalVariable, variable)
            )
        },
        elseCase = elseCase?.let {
            it.copy(
                action = it.action.useInlineVariable(globalVariable, variable),
            )
        }
    )

    is When.Single -> copy(
        expr = expr.useInlineVariable(globalVariable, variable),
        cases = cases.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable),
                action = it.action.useInlineVariable(globalVariable, variable)
            )
        },
        elseCase = elseCase?.let {
            it.copy(
                action = it.action.useInlineVariable(globalVariable, variable),
            )
        }
    )

    is GlobalVariable -> if (this == globalVariable) variable else this
    is ObjectVariable -> this
}

@JvmName("useInlineVariableExpressionList")
private fun List<Expression>.useInlineVariable(globalVariable: GlobalVariable, variable: Variable): List<Expression> =
    map {
        it.useInlineVariable(globalVariable, variable)
    }

@JvmName("useInlineVariableStatementList")
private fun List<Statement>.useInlineVariable(globalVariable: GlobalVariable, variable: Variable): List<Statement> =
    map {
        it.useInlineVariable(globalVariable, variable)
    }

private fun Statement.useInlineVariable(globalVariable: GlobalVariable, variable: Variable): Statement = when (this) {
    is Assignment -> copy(
        declaration = if (declaration == globalVariable.declaration) variable.target else declaration,
        newValue = newValue.useInlineVariable(globalVariable, variable)
    )
    is Add -> copy(
        declaration = if (declaration == globalVariable.declaration) variable.target else declaration,
        valueToAdd = valueToAdd.useInlineVariable(globalVariable, variable)
    )

    is Declaration.ObjectDeclaration -> copy(value = value?.useInlineVariable(globalVariable, variable))
    is Declaration.BooleanDeclaration -> copy(
        value = value?.useInlineVariable(
            globalVariable,
            variable
        ) as BooleanExpression?
    )

    is Declaration.DoubleDeclaration -> copy(
        value = value?.useInlineVariable(
            globalVariable,
            variable
        ) as NumberExpression.DoubleExpression?
    )

    is Declaration.IntDeclaration -> copy(
        value = value?.useInlineVariable(
            globalVariable,
            variable
        ) as NumberExpression.IntExpression?
    )

    is Declaration.StringDeclaration -> copy(
        value = value?.useInlineVariable(
            globalVariable,
            variable
        ) as StringExpression?
    )

    is DoWhile -> copy(
        functionCall = ((functionCall as Statement).useInlineVariable(globalVariable, variable)) as FunctionCall,
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression
    )

    is Exit -> this
    is For -> copy(
        counter = counter.useInlineVariable(globalVariable, variable) as Declaration.NumberDeclaration,
        from = from.useInlineVariable(globalVariable, variable) as NumberExpression,
        step = step?.useInlineVariable(globalVariable, variable) as NumberExpression?,
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
        statements = statements.useInlineVariable(globalVariable, variable)
    )

    is ForEach -> copy(
        variable = this.variable.useInlineVariable(globalVariable, variable) as Declaration,
        provider = provider.useInlineVariable(globalVariable, variable),
        statements = statements.useInlineVariable(globalVariable, variable)
    )

    is FunctionCall -> copy(
        parameters = parameters.useInlineVariable(globalVariable, variable)
    )

    is If -> copy(
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
        statements = statements.useInlineVariable(globalVariable, variable),
        elseStatements = elseStatements.useInlineVariable(globalVariable, variable),
        elseIfs = elseIfs.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
                statements = it.statements.useInlineVariable(globalVariable, variable)
            )
        }
    )

    is LoadExternal -> this
    is Print -> copy(expr = expr.useInlineVariable(globalVariable, variable) as StringExpression)
    is Return -> copy(expr = expr.useInlineVariable(globalVariable, variable))
    is Static -> this
    is Use -> copy(
        target = target.useInlineVariable(globalVariable, variable),
        action = action.useInlineVariable(globalVariable, variable)
    )

    is StringExpression.StringVariable.Use -> this
    is NumberExpression.IntExpression.IntVariable.Use -> this
    is NumberExpression.DoubleExpression.DoubleVariable.Use -> this
    is When.Multiple -> copy(
        cases = cases.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
                action = it.action.useInlineVariable(globalVariable, variable)
            )
        },
        elseCase = elseCase?.let {
            it.copy(
                action = it.action.useInlineVariable(globalVariable, variable),
            )
        }
    )

    is When.Single -> copy(
        expr = expr.useInlineVariable(globalVariable, variable),
        cases = cases.map {
            it.copy(
                condition = it.condition.useInlineVariable(globalVariable, variable),
                action = it.action.useInlineVariable(globalVariable, variable)
            )
        },
        elseCase = elseCase?.let {
            it.copy(
                action = it.action.useInlineVariable(globalVariable, variable),
            )
        }
    )

    is While -> copy(
        condition = condition.useInlineVariable(globalVariable, variable) as BooleanExpression,
        statements = statements.useInlineVariable(globalVariable, variable)
    )
}
