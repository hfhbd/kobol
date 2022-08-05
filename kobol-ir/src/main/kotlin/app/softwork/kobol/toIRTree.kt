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
    val dataTypes = data?.workingStorage?.map { it.toIR() } ?: emptyList()
    val external = procedure.topLevel.mapNotNull {
        when (it) {
            is Call -> it
            else -> null
        }
    } + procedure.sections.flatMap {
        it.statements.mapNotNull {
            when (it) {
                is Call -> it
                else -> null
            }
        }
    }
    val externalIR = external.toIR()

    val (main, functions) = procedure.functions(dataTypes + externalIR)
    return KobolIRTree(
        name = id.programID.toKotlinName(),
        main = main,
        types = functions + dataTypes + externalIR
    )
}

private fun List<Call>.toIR(): List<Class> = buildMap<String, Class> {
    for (call in this@toIR) {
        val name = call.name
        val old = this[name]
        val newFunction = call.toIrFunctionDeclaration()
        if (old == null) {
            this[name] = Class(
                name = call.name,
                constructor = Class.Constructor(emptyList()),
                members = emptyList(),
                functions = listOf(newFunction),
                doc = emptyList(),
                init = listOf(LoadExternal(call.name))
            )
        } else if (newFunction !in old.functions) {
            this[name] = old.copy(functions = old.functions + newFunction)
        }
    }
}.values.toList()

private fun Call.toIrFunctionDeclaration(): Types.Function {
    return Types.Function(
        name = name,
        parameters = emptyList(),
        returnType = Void,
        private = false,
        external = true,
        doc = comments,
        body = emptyList()
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
    is Call -> FunctionCall(
        function = (types.single {
            when (it) {
                is Class -> it.name == name
                else -> false
            }
        } as Class).functions.single {
            it.name == name
        },
        parameters = emptyList(),
        comments = comments
    )
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
