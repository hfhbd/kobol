package app.softwork.kobol

import app.softwork.kobol.KobolIRTree.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import java.io.*

fun File.toIR() = toTree().toIRTree()

fun CobolFIRTree.toIRTree(): KobolIRTree {
    val types = data?.workingStorage?.map { it.toIR() } ?: emptyList()
    val (main, functions) = functions(types)
    return KobolIRTree(
        name = id.programID.toKotlinName(),
        main = main,
        types = functions + types
    )
}

internal fun String.toKotlinName(): String = lowercase().replace("-", "_")

private fun CobolFIRTree.functions(types: List<Types.Type>): Pair<Types.Function, List<Types.Function>> {
    val sections = procedure.sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Types.Type.Void,
            body = emptyList(),
            private = false
        )
    }

    val topLevelStatements = procedure.topLevel.map { it.toIR(types, sections) }

    val main = Types.Function(
        name = "main",
        parameters = emptyList(),
        returnType = Types.Type.Void,
        body = topLevelStatements + sections.map {
            FunctionCall(it, parameters = emptyList())
        },
        private = false
    )
    val sectionsWithResolvedCalls = procedure.sections.map {
        Types.Function(
            name = it.name,
            parameters = emptyList(),
            returnType = Types.Type.Void,
            body = it.statements.map { it.toIR(types, sections) },
            private = false
        )
    }

    return main to sectionsWithResolvedCalls
}

fun CobolFIRTree.ProcedureTree.Expression.toIR(): Expression = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.StringExpression -> toIR()
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
                (target.toIR() as Types.Type.GlobalVariable).declaration as Declaration.StringDeclaration
            )
    }


fun CobolFIRTree.ProcedureTree.Statement.toIR(
    types: List<Types.Type>,
    sections: List<Types.Function>
): Types.Function.Statement = when (this) {
    is CobolFIRTree.ProcedureTree.Statement.Move -> {
        val declaration = types.mapNotNull { type ->
            when (type) {
                is Types.Type.GlobalVariable -> if (type.declaration.name == target.name) {
                    type.declaration
                } else null

                else -> null
            }
        }.single()
        Assignment(
            declaration = declaration,
            newValue = value.toIR()
        )
    }

    is CobolFIRTree.ProcedureTree.Statement.Display -> {
        Print(
            expr = expr.toIR()
        )
    }

    is CobolFIRTree.ProcedureTree.Statement.Perform -> {
        FunctionCall(
            function = sections.single { section -> sectionName == section.name },
            parameters = emptyList()
        )
    }
}

fun CobolFIRTree.DataTree.WorkingStorage.toIR(): Types.Type = when (this) {
    is CobolFIRTree.DataTree.WorkingStorage.Elementar -> {
        when (this) {
            is CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar -> {
                Types.Type.GlobalVariable(
                    declaration = Declaration.StringDeclaration(
                        name = name, value = value?.let {
                            Expression.StringExpression.StringLiteral(it)
                        },
                        mutable = true,
                        private = false
                    ),
                    const = false
                )
            }
        }
    }
}
