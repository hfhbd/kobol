package app.softwork.kobol.generator

import app.softwork.kobol.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import com.squareup.kotlinpoet.*
import java.io.*

object KotlinGenerator {
    fun generate(file: File): FileSpec {
        val tree = file.toTree()!!
        val name = tree.id.programID.lowercase()
        val fileSpec = FileSpec.builder(name, name)

        val data = tree.data
        if (data != null) {
            fileSpec.addData(data)
        }

        fileSpec.addFunction(
            FunSpec.builder("main").apply {
                addProcedure(tree.procedure)
            }.build()
        ).build()
        return fileSpec.build()
    }

    private fun FunSpec.Builder.addProcedure(procedure: CobolFIRTree.ProcedureTree) {
        procedure.topLevelStatements?.forEach {
            when (it) {
                is Move -> {
                    move(it)
                }

                is Display -> {
                    println(it)
                }
            }
        }
    }

    private fun FunSpec.Builder.move(it: Move) {
        addStatement("%N = %L", it.target.name, it.value.toKotlin())
    }

    private fun FunSpec.Builder.println(it: Display) {
        when (val expr = it.expr) {
            is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> {
                addStatement("println(%S)", expr.value)
            }

            is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> {
                addStatement("println(${expr.target.name})")
            }

            is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> {
                val template: String = expr.toTemplate()
                addStatement("println(\"%L\")", template)
            }
        }
    }

    private fun CobolFIRTree.ProcedureTree.Expression.toKotlin(): Any = when (this) {
        is CobolFIRTree.ProcedureTree.Expression.StringExpression -> "\"${toTemplate()}\""
    }

    private fun CobolFIRTree.ProcedureTree.Expression.StringExpression.toTemplate(): String = when (this) {
        is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> {
            value
        }

        is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> {
            "$${target.name}"
        }

        is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> {
            "${left.toTemplate()}${right.toTemplate()}"
        }
    }

    private fun FileSpec.Builder.addData(data: CobolFIRTree.DataTree) {
        data.workingStorage?.forEach {
            when (it) {
                is Elementar -> {
                    when (it) {
                        is Elementar.StringElementar -> {
                            val type = if (it.value == null) STRING.copy(nullable = true) else STRING
                            addProperty(
                                PropertySpec.builder(
                                    name = it.name,
                                    type = type
                                ).mutable(true)
                                    .initializer(codeBlock = CodeBlock.of("%S", it.value))
                                    .build()
                            )
                        }
                    }
                }
            }
        }
    }
}

fun KotlinGenerator.generate(file: File, output: File) {
    generate(file).writeTo(output)
}
