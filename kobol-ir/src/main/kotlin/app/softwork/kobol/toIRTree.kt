package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.KobolIRTree.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Type.*
import java.io.*

fun File.toIR() = toTree().toIRTree()
fun Set<File>.toIR() = toTree().map {
    it.toIRTree()
}

fun CobolFIRTree.toIRTree(): KobolIRTree {
    val types = data?.workingStorage?.map { it.toIR() } ?: emptyList()
    val (main, functions) = procedure.functions(types)
    return KobolIRTree(
        name = id.programID.toKotlinName(),
        main = main,
        types = functions + types
    )
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
        name = "main",
        parameters = emptyList(),
        returnType = Void,
        body = topLevelStatements + sections.map {
            FunctionCall(
                it,
                parameters = emptyList(),
                comments = emptyList()
            )
        },
        private = false,
        doc = comments
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

fun CobolFIRTree.ProcedureTree.Expression.toIR(): Expression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression -> toIR()
    is CobolFIRTree.ProcedureTree.Expression.NumberExpression -> TODO()
}

fun CobolFIRTree.ProcedureTree.Expression.StringExpression.toIR(): Expression.StringExpression =
    when (this) {
        is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> Expression.StringExpression.StringLiteral(
            value = value
        )

        is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat ->
            Expression.StringExpression.Concat(
                left = left.toIR(),
                right = right.toIR()
            )

        is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable ->
            Expression.StringExpression.StringVariable(
                (target.toIR() as GlobalVariable).declaration as Declaration.StringDeclaration
            )
    }


fun CobolFIRTree.ProcedureTree.Statement.toIR(
    types: List<Types.Type>,
    sections: List<Types.Function>
): Types.Function.Statement? = when (this) {
    is Move -> {
        val declaration = types.mapNotNull { type ->
            when (type) {
                is GlobalVariable -> if (type.declaration.name == target.name) {
                    type.declaration
                } else null

                else -> null
            }
        }.single()

        Assignment(
            declaration = declaration,
            newValue = value.toIR(),
            comments = comments
        )
    }

    is Display -> {
        Print(
            expr = expr.toIR(),
            comments = comments
        )
    }

    is Perform -> {
        FunctionCall(
            function = sections.single { section -> sectionName == section.name },
            parameters = emptyList(),
            comments = comments
        )
    }

    is ForEach -> TODO()
    is Continue -> null
    is GoBack -> Exit(comments)
}

fun CobolFIRTree.DataTree.WorkingStorage.toIR(): Types.Type = when (this) {
    is CobolFIRTree.DataTree.WorkingStorage.Elementar -> {
        when (this) {
            is CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar -> {
                GlobalVariable(
                    declaration = Declaration.StringDeclaration(
                        name = name, value = value?.let {
                            Expression.StringExpression.StringLiteral(it)
                        },
                        mutable = true,
                        private = false,
                        comments = comments
                    ),
                    const = false,
                    doc = comments
                )
            }

            is CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar -> TODO()
            is CobolFIRTree.DataTree.WorkingStorage.Elementar.Pointer -> TODO()
            is CobolFIRTree.DataTree.WorkingStorage.Elementar.EmptyElementar -> TODO()
        }
    }

    is CobolFIRTree.DataTree.WorkingStorage.Record -> TODO()
}
