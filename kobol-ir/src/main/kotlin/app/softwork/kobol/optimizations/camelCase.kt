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

    is KobolIRTree.Types.Function.Statement.FunctionCall -> toCamelCase()

    is KobolIRTree.Types.Function.Statement.Print -> copy(
        expr = expr.toCamelCase() as KobolIRTree.Expression.StringExpression
    )

    is KobolIRTree.Types.Function.Statement.Exit -> this
    is KobolIRTree.Types.Function.Statement.LoadExternal -> this
    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> copy(
        name = name.toCamelCase()
    )

    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> copy(
        name = name.toCamelCase()
    )

    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> copy(
        name = name.toCamelCase()
    )

    is KobolIRTree.Types.Function.Statement.DoWhile -> copy(
        functionCall = functionCall.toCamelCase(),
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression
    )

    is KobolIRTree.Types.Function.Statement.ForEach -> copy(
        counter = counter.updateNames() as KobolIRTree.Types.Function.Statement.Declaration.NumberDeclaration,
        from = from.toCamelCase() as KobolIRTree.Expression.NumberExpression,
        step = step?.toCamelCase() as KobolIRTree.Expression.NumberExpression?,
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
        statements = statements.map { it.updateNames() }
    )
    is KobolIRTree.Types.Function.Statement.While -> copy(
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
        statements = statements.map { it.updateNames() }
    )
}

private fun KobolIRTree.Types.Function.Statement.FunctionCall.toCamelCase() = copy(
    function = function.copy(name = function.name.toCamelCase())
)

private fun KobolIRTree.Types.Function.Statement.Declaration.updateName(): KobolIRTree.Types.Function.Statement.Declaration {
    return when (this) {
        is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> updateName()
        is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> updateName()
        is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> updateName()
        is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> updateName()
    }
}

private fun KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration.updateName() =
    copy(name = name.toCamelCase())

private fun KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration.updateName() =
    copy(name = name.toCamelCase())

private fun KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration.updateName() =
    copy(name = name.toCamelCase())

private fun KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration.updateName() =
    copy(name = name.toCamelCase())

private fun KobolIRTree.Expression.toCamelCase(): KobolIRTree.Expression =
    when (this) {
        is KobolIRTree.Expression.StringExpression.StringVariable -> copy(
            target = target.updateName()
        )

        is KobolIRTree.Expression.StringExpression.Concat -> copy(
            left = left.toCamelCase(),
            right = right.toCamelCase()
        )

        is KobolIRTree.Expression.Literal -> this
        is KobolIRTree.Types.Function.Statement.FunctionCall -> TODO()

        is KobolIRTree.Expression.BooleanExpression.And -> TODO()
        is KobolIRTree.Expression.BooleanExpression.Bigger -> TODO()
        is KobolIRTree.Expression.BooleanExpression.Eq -> copy(
            left = left.toCamelCase(), right = right.toCamelCase()
        )
        is KobolIRTree.Expression.BooleanExpression.NotEq -> TODO()
        is KobolIRTree.Expression.BooleanExpression.Not -> copy(
            condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression
        )
        is KobolIRTree.Expression.BooleanExpression.Or -> TODO()
        is KobolIRTree.Expression.BooleanExpression.Smaller -> TODO()
        is KobolIRTree.Types.Function.Statement.DoWhile -> TODO()
        is KobolIRTree.Types.Function.Statement.ForEach -> TODO()
        is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable -> copy(
            target = target.updateName()
        )
        is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable -> copy(
            target = target.updateName()
        )
        is KobolIRTree.Types.Function.Statement.While -> TODO()
        is KobolIRTree.Expression.StringExpression.Interpolation -> copy(expr.toCamelCase())
    }

private val next = "-(.)".toRegex()

private fun String.toCamelCase() = lowercase().replace(next) {
    it.groups[1]!!.value.uppercase()
}

private fun String.toPascalCase() = toCamelCase().replaceFirstChar { it.uppercase() }

private fun String.toSnakeCase() = toKotlinName().uppercase()
