package app.softwork.kobol.generator

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Expression.BooleanExpression.*

fun KobolIRTree.whenToIf() = copy(
    main = main.whenToIf(),
    types = types.map { it.whenToIf() }
)

private fun KobolIRTree.Types.Function.whenToIf() = copy(body = body.map {
    it.whenToIf()
})

private fun KobolIRTree.Types.Function.Statement.whenToIf() = when (this) {
    is KobolIRTree.Types.Function.Statement.When.Multiple -> {
        val elseIfs = if (cases.size == 1) emptyList() else
            cases.drop(1).map {
                KobolIRTree.Types.Function.Statement.If.ElseIf(
                    condition = it.condition,
                    statements = it.action,
                    comments = it.comments
                )
            }

        KobolIRTree.Types.Function.Statement.If(
            condition = cases.first().condition,
            statements = cases.first().action,
            elseIfs = elseIfs,
            elseStatements = elseCase?.action ?: emptyList(),
            comments = comments
        )
    }

    is KobolIRTree.Types.Function.Statement.When.Single -> {
        val elseIfs = if (cases.size == 1) emptyList() else
            cases.drop(1).map {
                KobolIRTree.Types.Function.Statement.If.ElseIf(
                    condition = Eq(expr, it.condition),
                    statements = it.action,
                    comments = it.comments
                )
            }

        KobolIRTree.Types.Function.Statement.If(
            condition = Eq(expr, cases.first().condition),
            statements = cases.first().action,
            elseIfs = elseIfs,
            elseStatements = elseCase?.action ?: emptyList(),
            comments = comments
        )
    }

    else -> this
}

private fun KobolIRTree.Types.whenToIf() = when (this) {
    is KobolIRTree.Types.Function -> whenToIf()
    is KobolIRTree.Types.Type.Class -> copy(
        init = init.map {
            it.whenToIf()
        },
        functions = functions.map {
            it.whenToIf()
        }
    )

    else -> this
}
