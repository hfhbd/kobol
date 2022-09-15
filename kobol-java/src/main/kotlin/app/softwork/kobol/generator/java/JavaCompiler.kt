package app.softwork.kobol.generator.java

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.optimizations.*
import com.squareup.javapoet.*
import java.io.*
import javax.lang.model.element.*
import javax.lang.model.element.Modifier.*

fun generate(tree: KobolIRTree, java8: Boolean): List<JavaFile> {
    val tree = if (java8) {
        tree.whenToIf()
    } else tree
    return generateJava(tree)
}

private fun generateJava(tree: KobolIRTree): List<JavaFile> {
    val external = tree.types.mapNotNull {
        if (it is KobolIRTree.Types.Type.Class && it.isObject) {
            it
        } else null
    }
    val main = tree.main.main(tree.name)
    tree.types.forEach {
        if (it !in external) {
            main.addType(it)
        }
    }
    val fileSpec = JavaFile.builder(tree.name, main.build())
    fileSpec.skipJavaLangImports(true)

    val externalTypes = external.map {
        JavaFile.builder(tree.name, it.toJava()).skipJavaLangImports(true)
    }

    return (externalTypes + fileSpec).map { it.build() }
}

private fun KobolIRTree.Types.Function.main(className: String): TypeSpec.Builder {
    return TypeSpec.classBuilder(className.replaceFirstChar { it.uppercaseChar() }).apply {
        val main = MethodSpec.methodBuilder("main").apply {
            addModifiers(PUBLIC, STATIC)
            returns(TypeName.VOID)
            addParameter(Array<String>::class.java, "args")
            body.forEach {
                addCode(it.toJava())
            }
        }
        addMethod(main.build())
        addModifiers(PUBLIC)
    }
}

private fun KobolIRTree.Types.Function.toJava(): MethodSpec {
    val name = if (external) "invoke" else name
    return MethodSpec.methodBuilder(name).apply {
        addModifiers(STATIC)
        if (private) {
            addModifiers(PRIVATE)
        } else {
            addModifiers(PUBLIC)
        }
        this@toJava.parameters.forEach {
            addParameter(it.Type, it.name)
        }
        if (external) {
            addModifiers(NATIVE)
        } else {
            body.forEach {
                addCode(it.toJava())
            }
            addJavadoc(doc.joinToString(separator = "\n"))
        }
    }.build()
}

fun CodeBlock.Builder.addComment(format: String): CodeBlock.Builder = apply {
    add("// $format\n")
}

@Suppress("NOTHING_TO_INLINE")
private inline fun KobolIRTree.Types.Function.Statement.toJava2(): CodeBlock = toJava()

private fun KobolIRTree.Types.Function.Statement.toJava() = CodeBlock.builder().let { code ->
    if (this !is Declaration && comments.isNotEmpty()) {
        for (comment in comments) {
            code.addComment(comment)
        }
    }
    when (this) {
        is Assignment -> code.addStatement("\$L = \$L", declaration.member(), this.newValue.toTemplate())

        is Print -> code.println(this)
        is Declaration -> code.add(CodeBlock.of("\$L", createProperty()))
        is FunctionCall -> code.add(call())
        is Exit -> code.addStatement("System.exit(0)")
        is LoadExternal -> code.addStatement("System.loadLibrary(\"$libName\")")
        is DoWhile -> code.beginControlFlow("do").add(functionCall.call()).unindent()
            .add("} while (\$L);\n", condition.toTemplate())

        is ForEach -> {
            val init = CodeBlock.of("\$L = \$L", counter.name, from.toTemplate())
            val condition = condition.toTemplate()
            val step = step
            val stepBlock = if (step != null) {
                CodeBlock.of("\$L += \$L", counter.name, step.toTemplate())
            } else CodeBlock.of("\$L++", counter.name)

            code.beginControlFlow("for (\$L; \$L; \$L)", init, condition, stepBlock)
            for (stmt in statements) {
                code.add(stmt.toJava2())
            }
            code.endControlFlow()
        }

        is While -> {
            code.beginControlFlow("while (\$L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toJava2()
                code.add(add)
            }
            code.endControlFlow()
        }

        is If -> {
            code.beginControlFlow("if (\$L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toJava2()
                code.add(add)
            }
            if (elseStatements.isNotEmpty()) {
                code.nextControlFlow("else")
                for (stmt in elseStatements) {
                    val add = stmt.toJava2()
                    code.add(add)
                }
            }
            code.endControlFlow()
        }

        is When -> {
            when (this) {
                is When.Single -> code.beginControlFlow("when (\$L)", expr.toTemplate())
                is When.Multiple -> code.beginControlFlow("when")
            }

            for (case in cases) {
                when (case) {
                    is When.Single.Case -> code.beginControlFlow("\$L ->", case.condition.toTemplate())
                    is When.Multiple.Case -> code.beginControlFlow("\$L ->", case.condition.toTemplate())
                }

                for (stmt in case.action) {
                    code.add("\$L", stmt.toJava2())
                }
                code.endControlFlow()
            }
            val elseCase = elseCase
            if (elseCase != null) {
                code.beginControlFlow("else ->")
                for (stmt in elseCase.action) {
                    code.add("\$L", stmt.toJava2())
                }
                code.endControlFlow()
            }
            code.endControlFlow()
        }
    }
    code.build()
}

private fun CodeBlock.Builder.println(it: Print) {
    when (val expr = it.expr) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            addStatement("System.out.println(\$L)", expr.toTemplate())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            addStatement("System.out.println(\$L)", expr.target.member())
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            val template = expr.toTemplate()
            addStatement("System.out.println(\$L)", template)
        }

        is KobolIRTree.Expression.StringExpression.Interpolation -> {
            val template = expr.expr.toTemplate()
            addStatement("System.out.println(\$L)", template)
        }
    }
}

private fun FunctionCall.call() = CodeBlock.builder().apply {
    val params = parameters.joinToString { it.name }
    val method = MethodSpec.methodBuilder(function.name).build()
    if (function.external) {
        addStatement("\$N.invoke($params)", method)
    } else {
        addStatement("\$N($params)", method)
    }
}.build()

private fun KobolIRTree.Expression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate()
    is KobolIRTree.Expression.NumberExpression -> toTemplate()
    is KobolIRTree.Expression.BooleanExpression -> toTemplate()
    is FunctionCall -> call()
    is DoWhile -> TODO()
    is ForEach -> TODO()
    is While -> TODO()
    is If -> TODO()
    is When -> TODO()
}

private fun Declaration.member(): CodeBlock {
    val className = className
    return if (className != null) {
        CodeBlock.of("$className.$name")
    } else CodeBlock.of(name)
}

private fun KobolIRTree.Expression.Variable.toCodeBlock(): CodeBlock {
    val memberName = target.member()
    return CodeBlock.of("\$L", memberName)
}

private fun KobolIRTree.Expression.StringExpression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression.StringLiteral -> {
        CodeBlock.of("\$S", value)
    }

    is KobolIRTree.Expression.StringExpression.StringVariable -> toCodeBlock()

    is KobolIRTree.Expression.StringExpression.Concat -> {
        CodeBlock.of("\$L + \$L", left.toTemplate(), right.toTemplate())
    }

    is KobolIRTree.Expression.StringExpression.Interpolation -> expr.toTemplate()
}

private fun KobolIRTree.Expression.BooleanExpression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.BooleanExpression.And -> CodeBlock.of(
        "\$L && \$L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Bigger -> if (equals) {
        CodeBlock.of(
            "\$L >= \$L", left.toTemplate(), right.toTemplate()
        )
    } else {
        CodeBlock.of(
            "\$L > \$L", left.toTemplate(), right.toTemplate()
        )
    }

    is KobolIRTree.Expression.BooleanExpression.BooleanLiteral -> CodeBlock.of("\$L", value)
    is KobolIRTree.Expression.BooleanExpression.Eq -> {
        if (left is KobolIRTree.Expression.StringExpression || right is KobolIRTree.Expression.StringExpression) {
            CodeBlock.of(
                "\$L.equals(\$L)", left.toTemplate(), right.toTemplate()
            )
        } else {
            CodeBlock.of(
                "\$L == \$L", left.toTemplate(), right.toTemplate()
            )
        }
    }

    is KobolIRTree.Expression.BooleanExpression.Not -> CodeBlock.of(
        "!(\$L)", condition.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.NotEq -> {
        if (left is KobolIRTree.Expression.StringExpression || right is KobolIRTree.Expression.StringExpression) {
            CodeBlock.of(
                "!\$L.equals(\$L)", left.toTemplate(), right.toTemplate()
            )
        } else {
            CodeBlock.of(
                "\$L != \$L", left.toTemplate(), right.toTemplate()
            )
        }
    }

    is KobolIRTree.Expression.BooleanExpression.Or -> CodeBlock.of(
        "\$L || \$L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Smaller -> if (equals) {
        CodeBlock.of(
            "\$L <= \$L", left.toTemplate(), right.toTemplate()
        )
    } else {
        CodeBlock.of(
            "\$L < \$L", left.toTemplate(), right.toTemplate()
        )
    }
}

private fun KobolIRTree.Expression.NumberExpression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleLiteral -> CodeBlock.of("\$L", value)
    is KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral -> CodeBlock.of("\$L", value)
    is KobolIRTree.Expression.NumberExpression.NumberVariable -> toCodeBlock()
}

private fun TypeSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addMethod(data.toJava())
        is KobolIRTree.Types.Type.Class -> addType(data.toJava())
        is KobolIRTree.Types.Type.External -> TODO()
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun KobolIRTree.Types.Type.Class.toJava() = TypeSpec.classBuilder(name).apply {
    addModifiers(PUBLIC)
    addJavadoc(doc.joinToString("\n"))

    if (init.isNotEmpty()) {
        val initBlock = CodeBlock.builder()
        for (init in init) {
            initBlock.add(init.toJava())
        }
        if (isObject) {
            addStaticBlock(initBlock.build())
        } else {
            addInitializerBlock(initBlock.build())
        }
    }

    for (member in members) {
        addField(member.createProperty())
    }

    for (function in functions) {
        addMethod(function.toJava())
    }
}.build()

private fun TypeSpec.Builder.addGlobalVariable(data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration
    addField(declaration.createProperty())
}

private fun Declaration.createProperty(): FieldSpec {
    val (type, init) = when (this) {
        is StringDeclaration -> ClassName.get("java.lang", "String") to value?.toTemplate()
        is BooleanDeclaration, is DoubleDeclaration, is IntDeclaration -> {
            if (value != null) {
                Type to value?.toTemplate()
            } else {
                Type.box() to null
            }
        }
    }
    return FieldSpec.builder(type, name).apply {
        if (!mutable) {
            addModifiers(FINAL)
        }

        addModifiers(STATIC)

        if (private) {
            addModifiers(PRIVATE)
        } else {
            addModifiers(PUBLIC)
        }
        initializer(init ?: CodeBlock.of("null"))
        addJavadoc(comments.joinToString(separator = "\n"))
    }.build()
}

private val Declaration.Type: TypeName
    get() = when (this) {
        is StringDeclaration -> ClassName.get("java.lang", "String")
        is BooleanDeclaration -> TypeName.BOOLEAN
        is DoubleDeclaration -> TypeName.DOUBLE
        is IntDeclaration -> TypeName.INT
    }

fun generate(file: File, output: File, optimize: Boolean, java8: Boolean) {
    generate(setOf(file), output, optimize, java8)
}

fun generate(files: Set<File>, output: File, optimize: Boolean, java8: Boolean) {
    for (ir in files.toIR()) {
        val finished = if (optimize) {
            ir.optimize()
        } else ir

        generate(finished, java8).forEach {
            it.writeTo(File(output, "java"))
        }
    }
}
