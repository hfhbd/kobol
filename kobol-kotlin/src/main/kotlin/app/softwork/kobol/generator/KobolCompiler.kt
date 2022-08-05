package app.softwork.kobol.generator

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
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

        addFunction(tree.main.toKotlin())
    }
    return fileSpec.build()
}

private fun KobolIRTree.Types.Function.toKotlin() = FunSpec.builder(name).apply {
    if (private) {
        addModifiers(KModifier.PRIVATE)
    }
    this@toKotlin.parameters.forEach {
        addParameter(it.name, it.KType)
    }
    if (external) {
        addModifiers(KModifier.EXTERNAL)
    } else {
        body.forEach {
            addCode(addStatement(it))
        }
        addKdoc(doc.joinToString(separator = "\n"))
        if (body.any { it is Exit }) {
            returns(NOTHING)
        }
    }
}.build()

fun CodeBlock.Builder.addComment(format: String, vararg args: Any): CodeBlock.Builder = apply {
    add("//·${format.replace(' ', '·')}\n", *args)
}

private fun addStatement(it: KobolIRTree.Types.Function.Statement) = CodeBlock.builder().apply {
    if (it !is Declaration && it.comments.isNotEmpty()) {
        for (comment in it.comments) {
            addComment(comment)
        }
    }
    when (it) {
        is Assignment -> assign(it)
        is Print -> println(it)
        is Declaration -> add(CodeBlock.of("%L", it.createProperty()))
        is FunctionCall -> add(it.call())
        is Exit -> addStatement("return %M(0)", MemberName("kotlin.system", "exitProcess", true))
        is LoadExternal -> addStatement("System.loadLibrary(\"${it.libName}\")")
    }
}.build()

private fun CodeBlock.Builder.assign(it: Assignment) {
    addStatement("%N = %L", it.declaration.name, it.newValue.toKotlin())
}

private fun CodeBlock.Builder.println(it: Print) {
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

private fun FunctionCall.call() = CodeBlock.builder().apply {
    val params = parameters.joinToString { it.name }
    if (this@call.function.external) {
        add("«")
        add("%M().", MemberName("", function.name))
        add("%M($params)", MemberName("", function.name))
        add("\n»")
    } else {
        addStatement("%M($params)", MemberName("", function.name))
    }
}.build()

private fun KobolIRTree.Expression.toKotlin(): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate()
    is FunctionCall -> call()
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

    is FunctionCall -> TODO()
}

private fun FileSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addFunction(data.toKotlin())
        is KobolIRTree.Types.Type.Class -> addClass(data)
        is KobolIRTree.Types.Type.External -> TODO()
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun FileSpec.Builder.addClass(data: KobolIRTree.Types.Type.Class) {
    val classBuilder = TypeSpec.classBuilder(data.name)
    classBuilder.addKdoc(data.doc.joinToString("\n"))

    if (data.init.isNotEmpty()) {
        val initBlock = CodeBlock.builder()
        for (init in data.init) {
            initBlock.add(addStatement(init))
        }
        classBuilder.addInitializerBlock(initBlock.build())
    }

    for (function in data.functions) {
        classBuilder.addFunction(function.toKotlin())
    }

    addType(classBuilder.build())
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

private fun Declaration.createProperty(): PropertySpec {
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

private val Declaration.KType: TypeName
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
