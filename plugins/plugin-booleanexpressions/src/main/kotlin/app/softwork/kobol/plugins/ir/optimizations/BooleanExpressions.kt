package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.IrPlugin
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.kobol.ir.KobolIRTree.Expression
import app.softwork.kobol.ir.KobolIRTree.Expression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.BooleanExpression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.DoubleExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.StringVariable
import app.softwork.kobol.ir.KobolIRTree.Types
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import app.softwork.kobol.ir.KobolIRTree.Types.Type
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
public class BooleanExpressions : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree.copy(
            main = tree.main.booleanExpressions(),
            types = tree.types.map { it.booleanExpressions() },
        )
    }
}

private fun Types.booleanExpressions(): Types {
    return when (this) {
        is Types.Function -> booleanExpressions()
        is Type.Class -> copy(
            functions = functions.map {
                it.booleanExpressions()
            },
            init = init.map {
                it.booleanExpressions()
            },
            members = members.map {
                it.booleanExpressions()
            },
        )

        is Type.GlobalVariable -> copy(declaration = declaration.booleanExpressions())
        is Type.Natives -> this
    }
}

private fun Types.Function.booleanExpressions() = copy(
    body = body.map {
        it.booleanExpressions()
    }.toMutableList(),
)

private fun Statement.booleanExpressions(): Statement =
    when (this) {
        is Statement.Assignment -> copy(newValue = newValue.booleanExpressions())
        is Statement.Math -> copy(value = value.booleanExpressions())
        is Declaration -> booleanExpressions()
        is Statement.DoWhile -> booleanExpressions()
        is Statement.Exit -> this
        is Statement.Return -> copy(expr = expr.booleanExpressions())
        is Statement.Throw -> copy(expr = expr.booleanExpressions())
        is Statement.For -> booleanExpressions()
        is Statement.ForEach -> booleanExpressions()
        is Statement.FunctionCall -> booleanExpressions()

        is Statement.LoadExternal -> this
        is Statement.TryCatch -> copy(
            tryStmts = tryStmts.booleanExpressions(),
            catchBlocks = catchBlocks.map {
                it.copy(stmts = it.stmts.booleanExpressions())
            },
            finallyStmts = finallyStmts.booleanExpressions(),
        )
        is Statement.Print -> this
        is Statement.While -> booleanExpressions()
        is Statement.If -> booleanExpressions()
        is Statement.When -> booleanExpressions()
        is Statement.Static -> this
        is Statement.Use -> copy(
            target = target.booleanExpressions(),
            action = action.booleanExpressions(),
        )

        is StringVariable.Use -> copy(
            variable = variable.booleanExpressions() as StringVariable,
        )

        is IntExpression.IntVariable.Use -> copy(
            variable = variable.booleanExpressions() as IntExpression.IntVariable,
        )

        is DoubleExpression.DoubleVariable.Use -> copy(
            variable = variable.booleanExpressions() as DoubleExpression.DoubleVariable,
        )
    }

private fun List<Statement>.booleanExpressions() = map { it.booleanExpressions() }

private fun Statement.When.booleanExpressions(): Statement.When =
    when (this) {
        is Statement.When.Single -> copy(
            expr = expr.booleanExpressions(),
            cases = cases.map {
                it.copy(
                    condition = it.condition.booleanExpressions(),
                    action = it.action.booleanExpressions(),
                )
            },
            elseCase = elseCase?.let {
                it.copy(
                    action = it.action.booleanExpressions(),
                )
            },
        )

        is Statement.When.Multiple -> copy(
            cases = cases.map {
                it.copy(
                    condition = it.condition.booleanExpressions() as BooleanExpression,
                    action = it.action.booleanExpressions(),
                )
            },
            elseCase = elseCase?.let {
                it.copy(
                    action = it.action.booleanExpressions(),
                )
            },
        )
    }

private fun Declaration.booleanExpressions() = when (this) {
    is Declaration.BooleanDeclaration -> copy(
        value = value?.optimize(),
    )

    is Declaration.DoubleDeclaration -> this
    is Declaration.IntDeclaration -> this
    is Declaration.StringDeclaration -> this
    is Declaration.ObjectDeclaration -> this
    is Declaration.Array -> this
}

private fun Expression.booleanExpressions(): Expression = when (this) {
    is BooleanExpression -> optimize()
    is NumberExpression -> this
    is StringExpression -> this
    is Statement.FunctionCall -> booleanExpressions()
    is Statement.If -> booleanExpressions()
    is Statement.When -> booleanExpressions()
    is Type.GlobalVariable -> copy(declaration = declaration.booleanExpressions())
    is ObjectVariable -> this
    is Statement.Use -> copy(
        target = target.booleanExpressions(),
        action = action.booleanExpressions(),
    )
}

private fun Statement.DoWhile.booleanExpressions() = copy(
    condition = condition.optimize(),
    statements = statements.booleanExpressions(),
)

private fun Statement.For.booleanExpressions() = copy(
    counter = counter.booleanExpressions() as Declaration.NumberDeclaration,
    from = from.booleanExpressions() as NumberExpression,
    step = step?.booleanExpressions() as NumberExpression?,
    condition = condition.optimize(),
    statements = statements.map {
        it.booleanExpressions()
    },
)

private fun Statement.ForEach.booleanExpressions() = copy(
    variable = variable.booleanExpressions(),
    provider = provider.booleanExpressions(),
    statements = statements.map {
        it.booleanExpressions()
    },
)

private fun Statement.FunctionCall.booleanExpressions() = copy(
    parameters = parameters.map {
        it.booleanExpressions()
    },
)

private fun Statement.While.booleanExpressions() = copy(
    condition = condition.optimize(),
    statements = statements.booleanExpressions(),
)

private fun Statement.If.booleanExpressions() = copy(
    condition = condition.optimize(),
    statements = statements.booleanExpressions(),
    elseStatements = elseStatements.booleanExpressions(),
)

internal fun BooleanExpression.optimize(): BooleanExpression = when (this) {
    is BooleanLiteral -> this

    is Eq -> Eq(left.booleanExpressions(), right.booleanExpressions())

    is Not -> when (val condition = condition) {
        is BooleanLiteral -> BooleanLiteral(!condition.value)
        is Not -> condition

        is Eq -> {
            val left = condition.left.booleanExpressions()
            val right = condition.right.booleanExpressions()
            if (left is BooleanLiteral && right is BooleanLiteral) {
                val result = left.value != right.value
                BooleanLiteral(result)
            } else {
                NotEq(left, right)
            }
        }

        is NotEq -> {
            val left = condition.left.booleanExpressions()
            val right = condition.right.booleanExpressions()
            if (left is BooleanLiteral && right is BooleanLiteral) {
                val result = left.value == right.value
                BooleanLiteral(result)
            } else {
                Eq(left, right)
            }
        }

        is And -> {
            val left = condition.left.optimize()
            val right = condition.right.optimize()
            Or(Not(left), Not(right))
        }

        is Or -> {
            val left = condition.left.optimize()
            val right = condition.right.optimize()
            And(Not(left), Not(right))
        }

        is Bigger -> Smaller(condition.left, condition.right, equals = !condition.equals)
        is Smaller -> Bigger(condition.left, condition.right, equals = !condition.equals)
    }.optimize()

    is NotEq -> NotEq(left.booleanExpressions(), right.booleanExpressions())

    is And -> {
        val left = left.optimize()
        val right = right.optimize()
        if (left is BooleanLiteral && right is BooleanLiteral) {
            val result = left.value && right.value
            BooleanLiteral(result)
        } else {
            And(left, right)
        }
    }

    is Or -> {
        val left = left.optimize()
        val right = right.optimize()
        if (left is BooleanLiteral && right is BooleanLiteral) {
            val result = left.value || right.value
            BooleanLiteral(result)
        } else {
            Or(left, right)
        }
    }

    is Bigger -> this
    is Smaller -> this
}
