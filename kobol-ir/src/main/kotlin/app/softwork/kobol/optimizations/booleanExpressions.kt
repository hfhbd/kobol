package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Expression.*
import app.softwork.kobol.KobolIRTree.Expression.BooleanExpression.*

fun KobolIRTree.booleanExpressions(): KobolIRTree {
    return copy(
        main = main.booleanExpressions(),
        types = types.map { it.booleanExpressions() }
    )
}

private fun KobolIRTree.Types.booleanExpressions(): KobolIRTree.Types {
    return when (this) {
        is KobolIRTree.Types.Function -> booleanExpressions()
        is KobolIRTree.Types.Type.Class -> copy(
            functions = functions.map {
                it.booleanExpressions()
            },
            init = init.map {
                it.booleanExpressions()
            },
            members = members.map {
                it.booleanExpressions()
            }
        )

        is KobolIRTree.Types.Type.External -> this
        is KobolIRTree.Types.Type.GlobalVariable -> copy(declaration = declaration.booleanExpressions())
        KobolIRTree.Types.Type.Void -> this
    }
}

private fun KobolIRTree.Types.Function.booleanExpressions() = copy(
    body = body.map {
        it.booleanExpressions()
    }
)

private fun KobolIRTree.Types.Function.Statement.booleanExpressions(): KobolIRTree.Types.Function.Statement =
    when (this) {
        is KobolIRTree.Types.Function.Statement.Assignment -> copy(newValue = newValue.booleanExpressions())
        is KobolIRTree.Types.Function.Statement.Declaration -> booleanExpressions()
        is KobolIRTree.Types.Function.Statement.DoWhile -> booleanExpressions()
        is KobolIRTree.Types.Function.Statement.Exit -> this
        is KobolIRTree.Types.Function.Statement.ForEach -> booleanExpressions()
        is KobolIRTree.Types.Function.Statement.FunctionCall -> booleanExpressions()
        is KobolIRTree.Types.Function.Statement.LoadExternal -> this
        is KobolIRTree.Types.Function.Statement.Print -> this
        is KobolIRTree.Types.Function.Statement.While -> booleanExpressions()
    }

private fun KobolIRTree.Types.Function.Statement.Declaration.booleanExpressions() = when (this) {
    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> copy(
        value = value?.optimize()
    )

    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> this
    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> this
    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> this
}

private fun KobolIRTree.Expression.booleanExpressions(): KobolIRTree.Expression = when (this) {
    is BooleanExpression -> optimize()
    is NumberExpression -> this
    is StringExpression -> this
    is KobolIRTree.Types.Function.Statement.DoWhile -> booleanExpressions()
    is KobolIRTree.Types.Function.Statement.ForEach -> booleanExpressions()
    is KobolIRTree.Types.Function.Statement.FunctionCall -> booleanExpressions()
    is KobolIRTree.Types.Function.Statement.While -> booleanExpressions()
}

private fun KobolIRTree.Types.Function.Statement.DoWhile.booleanExpressions() = copy(
    functionCall = functionCall.booleanExpressions(),
    condition = condition.optimize()
)

private fun KobolIRTree.Types.Function.Statement.ForEach.booleanExpressions() = copy(
    counter = counter.booleanExpressions() as KobolIRTree.Types.Function.Statement.Declaration.NumberDeclaration,
    from = from.booleanExpressions() as NumberExpression,
    step = step?.booleanExpressions() as NumberExpression?,
    condition = condition.optimize(),
    statements = statements.map {
        it.booleanExpressions()
    }
)

private fun KobolIRTree.Types.Function.Statement.FunctionCall.booleanExpressions() = copy(
    parameters = parameters.map {
        it.booleanExpressions()
    }
)

private fun KobolIRTree.Types.Function.Statement.While.booleanExpressions() = copy(
    condition = condition.optimize(),
    statements = statements.map { it.booleanExpressions() }
)

internal fun BooleanExpression.optimize(): BooleanExpression = when (this) {
    is BooleanLiteral -> this

    is Eq -> Eq(left.booleanExpressions(), right.booleanExpressions())

    is Not -> when (condition) {
        is BooleanLiteral -> BooleanLiteral(!condition.value)
        is Not -> condition

        is Eq -> {
            val left = condition.left.booleanExpressions()
            val right = condition.right.booleanExpressions()
            if (left is BooleanLiteral && right is BooleanLiteral) {
                val result = left.value != right.value
                BooleanLiteral(result)
            } else NotEq(left, right)
        }
        is NotEq -> {
            val left = condition.left.booleanExpressions()
            val right = condition.right.booleanExpressions()
            if (left is BooleanLiteral && right is BooleanLiteral) {
                val result = left.value == right.value
                BooleanLiteral(result)
            } else Eq(left, right)
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
        } else And(left, right)
    }

    is Or -> {
        val left = left.optimize()
        val right = right.optimize()
        if (left is BooleanLiteral && right is BooleanLiteral) {
            val result = left.value || right.value
            BooleanLiteral(result)
        } else Or(left, right)
    }

    is Bigger -> this
    is Smaller -> this
}