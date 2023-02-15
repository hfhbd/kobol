package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import kotlinx.serialization.builtins.*
import kotlinx.serialization.json.*
import java.io.*

public class Statistics(private val outputFolder: File) : FirCodeGenerator {

    public var results: Map<String, Results> = mapOf()
        private set

    override fun generate(fir: Iterable<CobolFIRTree>) {
        results = fir.associate {
            val complexity = it.complexity()

            it.id.programID.lowercase() to Results(
                complexity = complexity
            )
        }
    }

    override fun close() {
        File(outputFolder, "results.json").apply {
            if (!exists()) {
                createNewFile()
            }
        }.writeText(Json.encodeToString(MapSerializer(String.serializer(), Results.serializer()), results))
    }
}

internal fun CobolFIRTree.complexity(): Map<String, Int> = buildMap {
    var calc = 1
    for (it in procedure.topLevel) {
        it.complexity { calc += 1 }
    }
    put("main", calc)

    for (it in procedure.sections) {
        var calc = 1
        for (it in it.statements) {
            it.complexity { calc += 1 }
        }
        put(it.name, calc)
    }
}

private fun Statement.complexity(action: () -> Unit) {
    when (this) {
        is GoBack,
        is Call, is Continue,
        is Move, is Add,
        is Perform,
        is Sql,
        is Read,
        is Open,
        is Write, is Close,
        is Display -> Unit

        is While -> {
            action()
            until.complexity(action)
            for (stmt in statements) {
                stmt.complexity(action)
            }
        }

        is If -> {
            action()
            condition.complexity(action)
            for (stmt in statements) {
                stmt.complexity(action)
            }
            for (elseStmt in elseStatements) {
                elseStmt.complexity(action)
            }
        }

        is Eval -> {
            repeat(values.size) {
                action()
            }
            for (condition in conditions) {
                for (stmt in condition.action) {
                    stmt.complexity(action)
                }
            }
            val other = other
            if (other != null) {
                for (stmt in other.action) {
                    stmt.complexity(action)
                }
            }
        }

        is ForEach -> {
            action()
            until.complexity(action)
            for (stmt in statements) {
                stmt.complexity(action)
            }
        }
    }
}

private fun Expression.complexity(action: () -> Unit) {
    when (this) {
        is Expression.BooleanExpression.And -> {
            action()
            left.complexity(action)
            right.complexity(action)
        }

        is Expression.BooleanExpression.Equals -> {
            left.complexity(action)
            right.complexity(action)
        }

        is Expression.BooleanExpression.Greater -> {
            left.complexity(action)
            right.complexity(action)
        }

        is Expression.BooleanExpression.Not -> {
            target.complexity(action)
        }

        is Expression.BooleanExpression.Or -> {
            action()
            left.complexity(action)
            right.complexity(action)
        }

        is Expression.BooleanExpression.Smaller -> {
            left.complexity(action)
            right.complexity(action)
        }

        is Expression.Literal,
        is Expression.Variable,
        is Expression.StringExpression.Concat,
        is Expression.StringExpression.Interpolation -> Unit
    }
}
