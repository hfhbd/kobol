package app.softwork.kobol.generator

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.optimizations.*
import com.squareup.kotlinpoet.*
import java.io.*

fun generate(tree: KobolIRTree): FileSpec {
    val name = tree.name
    val fileSpec = FileSpec.builder(name, name).apply {
        tree.types.forEach {
            addType(it)
        }

        addFunction(tree.main)
    }
    return fileSpec.build()
}

private fun FileSpec.Builder.addFunction(function: KobolIRTree.Types.Function) {
    addFunction(
        FunSpec.builder(function.name).apply {
            if (function.private) {
                addModifiers(KModifier.PRIVATE)
            }
            function.parameters.forEach {
                addParameter(it.name, it.KType)
            }
            function.body.forEach {
                if (it !is KobolIRTree.Types.Function.Statement.Declaration && it.comments.isNotEmpty()) {
                    for (comment in it.comments) {
                        addComment(comment)
                    }
                }
                when (it) {
                    is KobolIRTree.Types.Function.Statement.Assignment -> {
                        assign(it)
                    }

                    is KobolIRTree.Types.Function.Statement.Print -> {
                        println(it)
                    }

                    is KobolIRTree.Types.Function.Statement.Declaration -> addProperty(it.createProperty())
                    is KobolIRTree.Types.Function.Statement.FunctionCall -> addCode(it.call())
                }
            }
            addKdoc(function.doc.joinToString(separator = "\n"))
        }.build()
    )
}

private fun FunSpec.Builder.assign(it: KobolIRTree.Types.Function.Statement.Assignment) {
    addStatement("%N = %L", it.declaration.name, it.newValue.toKotlin())
}

private fun FunSpec.Builder.println(it: KobolIRTree.Types.Function.Statement.Print) {
    when (val expr = it.expr) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            addStatement("println(%L)", it.expr.toKotlin())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            addStatement("println(${expr.target.name})")
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            val template = expr.toTemplate()
            addStatement("println(\"%L\")", template)
        }
    }
}

private fun KobolIRTree.Types.Function.Statement.FunctionCall.call() = CodeBlock.builder().apply {
    add("%M", MemberName("", function.name))
    add(
        parameters.joinToString(prefix = "(", postfix = ")") {
            it.name
        }
    )
}.build()

private fun KobolIRTree.Expression.toKotlin(): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate()
    is KobolIRTree.Types.Function.Statement.FunctionCall -> call()
}

private fun KobolIRTree.Expression.toTemplate(escape: Boolean = true): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression.StringLiteral -> {
        if (escape) {
            CodeBlock.of("%S", value)
        } else {
            CodeBlock.of("%L", value)
        }
    }

    is KobolIRTree.Expression.StringExpression.StringVariable -> {
        if (escape) {
            CodeBlock.of("%L", target.name)
        } else {
            CodeBlock.of("$%L", target.name)
        }
    }

    is KobolIRTree.Expression.StringExpression.Concat -> {
        CodeBlock.of("%L%L", left.toTemplate(escape = false), right.toTemplate(escape = false))
    }

    is KobolIRTree.Types.Function.Statement.FunctionCall -> TODO()
}

private fun FileSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addFunction(data)
        is KobolIRTree.Types.Type.Class -> TODO()
        is KobolIRTree.Types.Type.External -> TODO()
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun FileSpec.Builder.addGlobalVariable(data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration

    addProperty(
        declaration.createProperty().toBuilder().apply {
            if (data.const) {
                addModifiers(KModifier.CONST)
            }
        }.build()
    )
}

private fun KobolIRTree.Types.Function.Statement.Declaration.createProperty(): PropertySpec {
    val (type, init) = when (this) {
        is StringDeclaration -> {
            val value = value
            if (value != null) {
                STRING to value.toKotlin()
            } else STRING.copy(nullable = true) to null
        }
    }
    return PropertySpec.builder(
        name = name,
        type = type
    ).apply {
        mutable(mutable)
        if (private) {
            addModifiers(KModifier.PRIVATE)
        }
        initializer(init)
        addKdoc(comments.joinToString(separator = "\n"))
    }.build()
}

private val KobolIRTree.Types.Function.Statement.Declaration.KType: TypeName
    get() = when (this) {
        is StringDeclaration -> STRING
    }

fun generate(file: File, output: File, optimize: Boolean) {
    generate(setOf(file), output, optimize)
}

fun generate(files: Set<File>, output: File, optimize: Boolean) {
    for (ir in files.toIR()) {
        val finished = if (optimize) {
            ir.optimize()
        } else ir

        generate(finished).writeTo(directory = output)
    }
}
