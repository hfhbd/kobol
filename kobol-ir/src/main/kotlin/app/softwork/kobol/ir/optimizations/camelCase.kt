package app.softwork.kobol.ir.optimizations

import app.softwork.kobol.ir.*

public val CamelCase: IrPlugin = IrPlugin { tree ->
    tree.copy(
        main = tree.main.copy(
            body = tree.main.body.map { it.updateNames() }
        ),
        types = tree.types.map {
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
}

private fun KobolIRTree.Types.Function.Statement.updateNames(): KobolIRTree.Types.Function.Statement = when (this) {
    is KobolIRTree.Types.Function.Statement.Use -> copy(
        target = target.updateNames(),
        action = action.updateNames()
    )

    is KobolIRTree.Types.Function.Statement.Static -> copy(type.copy(name = type.name.toPascalCase()))

    is KobolIRTree.Types.Function.Statement.Assignment -> copy(
        declaration = declaration.updateNames()
    )

    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> updateName()

    is KobolIRTree.Types.Function.Statement.FunctionCall -> toCamelCase()

    is KobolIRTree.Types.Function.Statement.Print -> copy(
        expr = expr.toCamelCase() as KobolIRTree.Expression.StringExpression
    )

    is KobolIRTree.Types.Function.Statement.Exit -> this
    is KobolIRTree.Types.Function.Statement.Return -> copy(expr = expr.toCamelCase())
    is KobolIRTree.Types.Function.Statement.LoadExternal -> this
    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> updateName()

    is KobolIRTree.Types.Function.Statement.DoWhile -> copy(
        functionCall = functionCall.toCamelCase(),
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression
    )

    is KobolIRTree.Types.Function.Statement.For -> copy(
        counter = counter.updateNames() as KobolIRTree.Types.Function.Statement.Declaration.NumberDeclaration,
        from = from.toCamelCase() as KobolIRTree.Expression.NumberExpression,
        step = step?.toCamelCase() as KobolIRTree.Expression.NumberExpression?,
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
        statements = statements.map { it.updateNames() }
    )

    is KobolIRTree.Types.Function.Statement.ForEach -> copy(
        variable = variable.updateNames() as KobolIRTree.Types.Function.Statement.Declaration,
        provider = provider.toCamelCase(),
        statements = statements.map { it.updateNames() }
    )

    is KobolIRTree.Types.Function.Statement.While -> copy(
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
        statements = statements.map { it.updateNames() }
    )

    is KobolIRTree.Types.Function.Statement.If -> copy(
        condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
        statements = statements.map { it.updateNames() },
        elseStatements = elseStatements.map { it.updateNames() }
    )

    is KobolIRTree.Types.Function.Statement.When -> toCamelCase()
    is KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration -> copy(name = name.toCamelCase())
    is KobolIRTree.Expression.StringExpression.StringVariable.Use -> TODO()
    is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> copy(
        variable = variable.toCamelCase() as KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable
    )

    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> copy(
        variable = variable.toCamelCase() as KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable
    )
}

private fun KobolIRTree.Types.Function.Statement.When.toCamelCase(): KobolIRTree.Types.Function.Statement.When {
    return when (this) {
        is KobolIRTree.Types.Function.Statement.When.Single -> copy(
            cases = cases.map {
                it.copy(
                    condition = it.condition.toCamelCase(),
                    action = it.action.map { it.updateNames() }
                )
            },
            elseCase = elseCase?.let {
                it.copy(
                    action = it.action.map {
                        it.updateNames()
                    }
                )
            }
        )

        is KobolIRTree.Types.Function.Statement.When.Multiple -> copy(
            cases = cases.map {
                it.copy(
                    condition = it.condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
                    action = it.action.map { it.updateNames() }
                )
            },
            elseCase = elseCase?.let {
                it.copy(
                    action = it.action.map {
                        it.updateNames()
                    }
                )
            }
        )
    }
}

private fun KobolIRTree.Types.Function.Statement.FunctionCall.toCamelCase() = copy(
    function = when (val function = function) {
        is KobolIRTree.Types.Function -> function.copy(name = function.name.toCamelCase())
        is KobolIRTree.Types.Type.Class -> function.copy(name = function.name.toPascalCase())
    }
)

private fun KobolIRTree.Types.Function.Statement.Declaration.updateName() = when (this) {
    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> updateName()
    is KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration -> copy(name = name.toCamelCase())
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
        is KobolIRTree.Types.Function.Statement.FunctionCall -> toCamelCase()
        is KobolIRTree.Types.Function.Statement.Use -> copy(
            target = target.updateNames(),
            action = action.updateNames(),
        )

        is KobolIRTree.Expression.BooleanExpression.And -> copy(
            left = left.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
            right = right.toCamelCase() as KobolIRTree.Expression.BooleanExpression
        )

        is KobolIRTree.Expression.BooleanExpression.Bigger -> copy(
            left = left.toCamelCase() as KobolIRTree.Expression.NumberExpression,
            right = right.toCamelCase() as KobolIRTree.Expression.NumberExpression
        )

        is KobolIRTree.Expression.BooleanExpression.Eq -> copy(
            left = left.toCamelCase(), right = right.toCamelCase()
        )

        is KobolIRTree.Expression.BooleanExpression.NotEq -> copy(
            left = left.toCamelCase(), right = right.toCamelCase()
        )

        is KobolIRTree.Expression.BooleanExpression.Not -> copy(
            condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression
        )

        is KobolIRTree.Expression.BooleanExpression.Or -> copy(
            left = left.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
            right = right.toCamelCase() as KobolIRTree.Expression.BooleanExpression
        )

        is KobolIRTree.Expression.BooleanExpression.Smaller -> copy(
            left = left.toCamelCase() as KobolIRTree.Expression.NumberExpression,
            right = right.toCamelCase() as KobolIRTree.Expression.NumberExpression
        )

        is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable -> copy(
            target = target.updateName()
        )

        is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable -> copy(
            target = target.updateName()
        )

        is KobolIRTree.Expression.StringExpression.Interpolation -> copy(expr.toCamelCase())
        is KobolIRTree.Types.Function.Statement.If -> copy(
            condition = condition.toCamelCase() as KobolIRTree.Expression.BooleanExpression,
            statements = statements.map { it.updateNames() },
            elseStatements = elseStatements.map { it.updateNames() }
        )

        is KobolIRTree.Types.Function.Statement.When -> toCamelCase()
        is KobolIRTree.Types.Type.GlobalVariable -> copy(
            declaration = declaration.updateName()
        )

        is KobolIRTree.Expression.ObjectVariable -> copy(
            target = target.updateName() as KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration
        )

        is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> TODO()
        is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> TODO()
        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> TODO()
    }

private val next = "-(.)".toRegex()

private fun String.toCamelCase() = lowercase().replace(next) {
    it.groups[1]!!.value.uppercase()
}

private fun String.toPascalCase() = toCamelCase().replaceFirstChar { it.uppercase() }

private fun String.toSnakeCase() = toKotlinName().uppercase()
