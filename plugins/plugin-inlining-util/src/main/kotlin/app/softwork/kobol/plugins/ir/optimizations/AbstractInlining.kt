package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.*
import app.softwork.kobol.ir.KobolIRTree.Expression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*

public abstract class AbstractInlining(private val syntheticOnly: Boolean) : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree.inlineGlobalVariables(syntheticOnly)
    }
}

/**
 * If a global variable is only used once, move it into this function.
 */
internal fun KobolIRTree.inlineGlobalVariables(syntheticOnly: Boolean): KobolIRTree {
    val globalVariables = types.mapNotNull {
        when {
            it is GlobalVariable && ((syntheticOnly && (it.target as? Declaration.Primitive)?.synthetic == true) || !syntheticOnly) -> it

            else -> null
        }
    }

    val removed = globalVariables.mapNotNull { globalVariable ->
        val writes = findWriteUsages(globalVariable.declaration)
        val singleWrite = when {
            writes.main == null && writes.types.isEmpty() -> null // never written
            writes.main != null && writes.types.isEmpty() -> writes.main!!
            writes.main == null && writes.types.size == 1 -> (writes.types.single() as? Class)?.functions?.singleOrNull()
                ?: return@mapNotNull null

            else -> return@mapNotNull null
        }

        val reads = findReadUsages(globalVariable.declaration)
        val singleRead = when {
            reads.main != null && reads.types.isEmpty() -> reads.main!!
            reads.main == null && reads.types.size == 1 -> (reads.types.single() as? Class)?.functions?.singleOrNull()
                ?: return@mapNotNull null

            else -> null
        }
        val isSynthetic = (globalVariable.target as? Declaration.Primitive)?.synthetic == true
        when {
            (singleWrite != null && singleWrite == singleRead) || (singleWrite == null && singleRead != null) -> {
                inline(
                    globalVariable = globalVariable,
                    noWrites = singleWrite == null,
                    writesAndReadsInSameFunction = singleWrite == singleRead,
                    singleRead = singleRead,
                    isSynthetic = isSynthetic
                )
            }

            isSynthetic -> {
                synthetic(globalVariable, reads)
            }

            else -> null
        }
    }
    return copy(
        types = types.filter {
            it !in removed
        }
    )
}

private fun inline(
    globalVariable: GlobalVariable,
    noWrites: Boolean,
    writesAndReadsInSameFunction: Boolean,
    singleRead: Types.Function,
    isSynthetic: Boolean
): GlobalVariable {
    val dec = globalVariable.declaration
    val expr = when {
        noWrites && isSynthetic -> (dec as Declaration.Primitive).value!!
        writesAndReadsInSameFunction -> {
            singleRead.body.add(0, dec)
            dec.variable()
        }
        noWrites -> {
            val nonMutableDec = when (dec) {
                is Declaration.ObjectDeclaration -> dec.copy(mutable = false)
                is Declaration.BooleanDeclaration -> dec.copy(mutable = false)
                is Declaration.DoubleDeclaration -> dec.copy(mutable = false)
                is Declaration.IntDeclaration.Normal -> dec.copy(mutable = false)
                is Declaration.IntDeclaration.ReturnCode -> dec.copy(mutable = false)
                is Declaration.StringDeclaration -> dec.copy(mutable = false)
            }
            singleRead.body.add(0, nonMutableDec)
            nonMutableDec.variable()
        }

        isSynthetic -> (dec as Declaration.Primitive).value!!
        else -> {
            singleRead.body.add(0, dec)
            dec.variable()
        }
    }

    singleRead.body.replaceAll {
        it.useInlineVariable(globalVariable, expr)
    }
    return globalVariable
}

private fun synthetic(
    globalVariable: GlobalVariable,
    reads: Usage
): GlobalVariable {
    val expr = (globalVariable.target as Declaration.Primitive).value!!
    reads.main?.body?.replaceAll {
        it.useInlineVariable(globalVariable, expr)
    }
    for (type in reads.types) {
        when (type) {
            is Types.Function -> {
                type.body.replaceAll {
                    it.useInlineVariable(globalVariable, expr)
                }
            }

            else -> Unit
        }
    }
    return globalVariable
}

private fun Expression.useInlineVariable(globalVariable: GlobalVariable, variable: Expression): Expression =
    when (this) {
        is Literal -> this
        is Variable -> if (target == globalVariable.target) variable else this
        is BooleanExpression.And -> copy(
            left = left.useInlineVariable(globalVariable, variable) as BooleanExpression,
            right = right.useInlineVariable(globalVariable, variable) as BooleanExpression
        )

        is BooleanExpression.Bigger -> copy(
            left = left.useInlineVariable(globalVariable, variable) as NumberExpression,
            right = right.useInlineVariable(globalVariable, variable) as NumberExpression
        )

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

        is NumberExpression.DoubleExpression.DoubleVariable.Use -> this
        is NumberExpression.IntExpression.IntVariable.Use -> this
        is StringExpression.Concat -> copy(
            left = left.useInlineVariable(globalVariable, variable),
            right = right.useInlineVariable(globalVariable, variable)
        )

        is StringExpression.Interpolation -> copy(
            expr = expr.useInlineVariable(globalVariable, variable),
        )

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
private fun List<Expression>.useInlineVariable(globalVariable: GlobalVariable, variable: Expression): List<Expression> =
    map {
        it.useInlineVariable(globalVariable, variable)
    }

@JvmName("useInlineVariableStatementList")
private fun List<Statement>.useInlineVariable(globalVariable: GlobalVariable, variable: Expression): List<Statement> =
    map {
        it.useInlineVariable(globalVariable, variable)
    }

private fun Statement.useInlineVariable(globalVariable: GlobalVariable, variable: Expression): Statement = when (this) {
    is Assignment -> copy(
        declaration = if (declaration == globalVariable.declaration) (variable as Variable).target else declaration,
        newValue = newValue.useInlineVariable(globalVariable, variable)
    )

    is Math -> copy(
        declaration = if (declaration == globalVariable.declaration) (variable as Variable).target else declaration,
        value = value.useInlineVariable(globalVariable, variable)
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

    is Declaration.IntDeclaration.Normal -> copy(
        value = value?.useInlineVariable(
            globalVariable,
            variable
        ) as NumberExpression.IntExpression?
    )
    is Declaration.IntDeclaration.ReturnCode -> copy(
        value = value.useInlineVariable(
            globalVariable,
            variable
        ) as NumberExpression.IntExpression
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

    is Exit -> copy(
        returnVariable = returnVariable.useInlineVariable(globalVariable, variable) as NumberExpression.IntExpression
    )

    is Throw -> copy(
        expr.useInlineVariable(globalVariable, variable)
    )

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
