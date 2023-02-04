package app.softwork.kobol.ir

import app.softwork.kobol.ir.KobolIRTree.Expression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*

public data class Usage(
    val main: KobolIRTree.Types.Function?,
    val types: List<KobolIRTree.Types>
)

public fun KobolIRTree.findWriteUsages(declaration: Declaration): Usage {
    val main = main.contains(declaration)
    val types = types.flatMap { type ->
        when (type) {
            is KobolIRTree.Types.Function -> {
                listOfNotNull(type.contains(declaration))
            }

            is KobolIRTree.Types.Type.Class -> {
                type.functions.mapNotNull { classFunction ->
                    classFunction.contains(declaration)
                }
            }

            is KobolIRTree.Types.Type.GlobalVariable, KobolIRTree.Types.Type.Void -> emptyList()
        }
    }
    return Usage(main, types)
}

public fun KobolIRTree.findReadUsages(
    declaration: Declaration,
): Usage {
    val main = main.takeIf {
        declaration usedIn it.body
    }

    val types: List<KobolIRTree.Types> = types.flatMap {
        when (it) {
            is KobolIRTree.Types.Function -> if (declaration usedIn it.body) {
                listOf(it)
            } else emptyList()

            is KobolIRTree.Types.Type.Class -> it.functions.mapNotNull {
                it.takeIf { declaration usedIn it.body }
            }

            is KobolIRTree.Types.Type.GlobalVariable, KobolIRTree.Types.Type.Void -> emptyList()
        }
    }
    return Usage(main, types)
}

/**
 * Returns null if there function does not write to [declaration]
 */
public fun KobolIRTree.Types.Function.contains(declaration: Declaration): KobolIRTree.Types.Function? {
    return takeIf { body assigns declaration }
}

private infix fun List<KobolIRTree.Types.Function.Statement>.assigns(declaration: Declaration) =
    any { it assigns declaration }

private infix fun KobolIRTree.Types.Function.Statement.assigns(declaration: Declaration): Boolean = when (this) {
    is Assignment -> this.declaration == declaration
    is Declaration -> false
    is DoWhile -> functionCall assigns declaration
    is Exit -> false
    is For -> counter == declaration || statements assigns declaration
    is ForEach -> variable == declaration || statements assigns declaration
    is FunctionCall -> when (function) {
        is KobolIRTree.Types.Function -> function.body assigns declaration
        is KobolIRTree.Types.Type.Class -> false
    }

    is If -> statements assigns declaration || elseIfs.any {
        it.statements assigns declaration
    } || elseStatements assigns declaration

    is LoadExternal -> false
    is Print -> false
    is Return -> false
    is Static -> false
    is Use -> target assigns declaration || action assigns declaration
    is StringExpression.StringVariable.Use -> false
    is NumberExpression.IntExpression.IntVariable.Use -> false
    is NumberExpression.DoubleExpression.DoubleVariable.Use -> false
    is When.Multiple -> cases.any {
        it.action assigns declaration
    } || (elseCase?.action?.assigns(declaration) ?: false)

    is When.Single -> cases.any {
        it.action assigns declaration
    } || (elseCase?.action?.assigns(declaration) ?: false)

    is While -> statements assigns declaration
}

private infix fun Declaration.usedIn(statements: List<KobolIRTree.Types.Function.Statement>?): Boolean {
    if (statements == null) {
        return false
    }
    return statements.any {
        this usedIn it
    }
}

private infix fun Declaration.usedIn(it: KobolIRTree.Types.Function.Statement): Boolean =
    when (it) {
        is Assignment -> this in it.newValue
        is Declaration -> this in it.value
        is DoWhile -> this in it.functionCall.parameters || this in it.condition
        is Exit -> false
        is For -> {
            this == it.counter || this in it.from || this in it.step || this in it.condition || this usedIn it.statements
        }

        is ForEach -> {
            this == it.provider || this in it.provider || this usedIn it.statements
        }

        is FunctionCall -> this in it.parameters

        is If -> this in it.condition || this usedIn it.statements || this usedIn it.elseStatements || it.elseIfs.any {
            this in it.condition || this usedIn it.statements
        }

        is LoadExternal -> false
        is Print -> this in it.expr
        is When.Multiple -> this usedIn it.elseCase?.action || it.cases.any {
            this in it.condition || this usedIn it.action
        }

        is When.Single -> this in it.expr || this usedIn it.elseCase?.action || it.cases.any {
            this in it.condition || this usedIn it.action
        }

        is While -> this in it.condition || this usedIn it.statements

        is Return -> this in it.expr
        is Static -> false
        is Use -> this usedIn it.target || this usedIn it.action
        is StringExpression.StringVariable.Use -> false
        is NumberExpression.IntExpression.IntVariable.Use -> false
        is NumberExpression.DoubleExpression.DoubleVariable.Use -> false
    }

private operator fun List<KobolIRTree.Expression>.contains(decl: Declaration): Boolean = any {
    it.contains(decl)
}

private operator fun KobolIRTree.Expression?.contains(declaration: Declaration): Boolean = when (this) {
    null -> false
    is BooleanExpression -> declaration in this
    is KobolIRTree.Types.Function.Statement -> declaration usedIn this
    is Literal -> false
    is Variable -> target == declaration
    is StringExpression.Concat -> declaration in left || declaration in right
    is StringExpression.Interpolation -> declaration in expr
}

private operator fun BooleanExpression.contains(declaration: Declaration): Boolean = when (this) {
    is BooleanExpression.And -> declaration in left || declaration in right
    is BooleanExpression.Bigger -> declaration in left || declaration in right
    is BooleanExpression.BooleanLiteral -> false
    is BooleanExpression.Eq -> declaration in left || declaration in right
    is BooleanExpression.Not -> declaration in condition
    is BooleanExpression.NotEq -> declaration in left || declaration in right
    is BooleanExpression.Or -> declaration in left || declaration in right
    is BooleanExpression.Smaller -> declaration in left || declaration in right
}
