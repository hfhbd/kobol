package app.softwork.kobol.optimizations

import app.softwork.kobol.*

fun KobolIRTree.camelCase(): KobolIRTree = copy(
    main = main.copy(body = main.body.map { it.updateNames() }),
    types = types.map {
        when (it) {
            is KobolIRTree.Types.Function -> it.copy(
                name = it.name.toCamelCase(),
                body = it.body.map {
                    it.updateNames()
                }
            )

            is KobolIRTree.Types.Type.Class -> it.copy(name = it.name.toPascalCase())
            is KobolIRTree.Types.Type.GlobalVariable -> it.copy(
                declaration = it.declaration.updateName()
            )

            else -> it
        }
    }
)

private fun KobolIRTree.Types.Function.Statement.updateNames(): KobolIRTree.Types.Function.Statement = when (this) {
    is KobolIRTree.Types.Function.Statement.Assignment -> copy(
        declaration = declaration.updateName()
    )

    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> copy(
        name = name.toCamelCase()
    )

    is KobolIRTree.Types.Function.Statement.FunctionCall -> copy(
        function = function.copy(name = function.name.toCamelCase())
    )

    is KobolIRTree.Types.Function.Statement.Print -> copy(
        expr = expr.toCamelCase()
    )

    is KobolIRTree.Types.Function.Statement.Exit -> this
    is KobolIRTree.Types.Function.Statement.LoadExternal -> this
}

private fun KobolIRTree.Types.Function.Statement.Declaration.updateName(): KobolIRTree.Types.Function.Statement.Declaration {
    return when (this) {
        is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> updateName()
    }
}

private fun KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration.updateName() =
    copy(name = name.toCamelCase())

private fun KobolIRTree.Expression.toCamelCase(): KobolIRTree.Expression.StringExpression =
    when (this) {
        is KobolIRTree.Expression.StringExpression.StringVariable -> copy(
            target = target.updateName()
        )

        is KobolIRTree.Expression.StringExpression.Concat -> copy(
            left = left.toCamelCase(),
            right = right.toCamelCase()
        )

        is KobolIRTree.Expression.StringExpression.StringLiteral -> this
        is KobolIRTree.Types.Function.Statement.FunctionCall -> TODO()
    }

private val next = "-(.)".toRegex()

private fun String.toCamelCase() = lowercase().replace(next) {
    it.groups[1]!!.value.uppercase()
}

private fun String.toPascalCase() = toCamelCase().replaceFirstChar { it.uppercase() }

private fun String.toSnakeCase() = toKotlinName().uppercase()
