package app.softwork.kobol.generator

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.optimizations.*
import com.squareup.javapoet.*
import java.io.*
import javax.lang.model.element.*
import javax.lang.model.element.Modifier.*

fun generate(tree: KobolIRTree): JavaFile {
    val main = tree.main.main(tree.name)
    tree.types.forEach {
        main.addType(it)
    }
    val fileSpec = JavaFile.builder(tree.name, main.build())
    return fileSpec.build()
}

private fun KobolIRTree.Types.Function.main(className: String): TypeSpec.Builder {
    return TypeSpec.classBuilder(className).apply {
        val main = MethodSpec.methodBuilder("main").apply {
            addModifiers(PUBLIC, STATIC)
            returns(TypeName.VOID)
            addParameter(Array<String>::class.java, "args")
            body.forEach {
                addStatement(it.toJava())
            }
        }
        addMethod(main.build())
    }
}

private fun KobolIRTree.Types.Function.toJava(): MethodSpec {
    val name = if (external) "invoke" else name
    return MethodSpec.methodBuilder(name).apply {
        if (private) {
            addModifiers(PRIVATE)
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

fun CodeBlock.Builder.addComment(format: String, vararg args: Any): CodeBlock.Builder = apply {
    add("//·${format.replace(' ', '·')}\n", *args)
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
        is Assignment -> code.assign(this)
        is Print -> code.println(this)
        is Declaration -> code.add(CodeBlock.of("%L", createProperty()))
        is FunctionCall -> code.add(call())
        is Exit -> code.addStatement("return System.exit(0);")
        is LoadExternal -> code.addStatement("System.loadLibrary(\"$libName\")")
        is DoWhile -> code.beginControlFlow("do").add(functionCall.call()).unindent()
            .add("} while (%L)\n", condition.toTemplate())

        is ForEach -> {
            code.add("%L = %L\n", counter.name, from.toTemplate())
            code.beginControlFlow("while (%L)", condition.toTemplate())
            for (stmt in statements) {
                code.add(stmt.toJava2())
            }
            val step = step
            if (step != null) {
                code.addStatement("%L += %L", counter.name, step.toTemplate())
            }
            code.endControlFlow()
        }

        is While -> {
            code.beginControlFlow("while (%L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toJava2()
                code.add(add)
            }
            code.endControlFlow()
        }

        is If -> {
            code.beginControlFlow("if (%L)", condition.toTemplate())
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
                is When.Single -> code.beginControlFlow("when (%L)", expr.toTemplate())
                is When.Multiple -> code.beginControlFlow("when")
            }

            for (case in cases) {
                when (case) {
                    is When.Single.Case -> code.beginControlFlow("%L ->", case.condition.toTemplate())
                    is When.Multiple.Case -> code.beginControlFlow("%L ->", case.condition.toTemplate())
                }

                for (stmt in case.action) {
                    code.add("%L", stmt.toJava2())
                }
                code.endControlFlow()
            }
            val elseCase = elseCase
            if (elseCase != null) {
                code.beginControlFlow("else ->")
                for (stmt in elseCase.action) {
                    code.add("%L", stmt.toJava2())
                }
                code.endControlFlow()
            }
            code.endControlFlow()
        }
    }
    code.build()
}

private fun CodeBlock.Builder.assign(it: Assignment) {
    addStatement("%L = %L", it.declaration.member(), it.newValue.toTemplate())
}

private fun CodeBlock.Builder.println(it: Print) {
    when (val expr = it.expr) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            addStatement("println(%L)", it.expr.toTemplate())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            addStatement("println(%L)", expr.target.member())
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            val template = expr.toTemplate()
            addStatement("println(\"%L\")", template)
        }

        is KobolIRTree.Expression.StringExpression.Interpolation -> {
            val template = expr.expr.toTemplate()
            addStatement("println(%L)", template)
        }
    }
}

private fun FunctionCall.call() = CodeBlock.builder().apply {
    val params = parameters.joinToString { it.name }
    addStatement("%M($params)", MemberName("", function.name))
}.build()

private fun KobolIRTree.Expression.toTemplate(escape: Boolean = true): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate(escape)
    is KobolIRTree.Expression.NumberExpression -> toTemplate(escape)
    is KobolIRTree.Expression.BooleanExpression -> toTemplate()
    is FunctionCall -> call()
    is DoWhile -> TODO()
    is ForEach -> TODO()
    is While -> TODO()
    is If -> TODO()
    is When -> TODO()
}

private fun Declaration.member(): MemberName {
    val className = className
    return if (className != null) {
        MemberName(ClassName("", className), name)
    } else MemberName("", name)
}

private fun KobolIRTree.Expression.Variable.toCodeBlock(escape: Boolean): CodeBlock {
    return if (escape) {
        CodeBlock.of("%L", target.member())
    } else {
        val memberName = target.member()
        if (memberName.enclosingClassName != null) {
            CodeBlock.of("${"$"}{%M}", memberName)
        } else CodeBlock.of("$%M", memberName)
    }
}

private fun KobolIRTree.Expression.StringExpression.toTemplate(escape: Boolean = true): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression.StringLiteral -> {
        if (escape) {
            CodeBlock.of("%S", value)
        } else {
            CodeBlock.of("%L", value)
        }
    }

    is KobolIRTree.Expression.StringExpression.StringVariable -> toCodeBlock(escape)

    is KobolIRTree.Expression.StringExpression.Concat -> {
        CodeBlock.of("%L%L", left.toTemplate(escape = false), right.toTemplate(escape = false))
    }

    is KobolIRTree.Expression.StringExpression.Interpolation -> expr.toTemplate(escape)
}

private fun KobolIRTree.Expression.BooleanExpression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.BooleanExpression.And -> CodeBlock.of(
        "%L && %L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Bigger -> if (equals) {
        CodeBlock.of(
            "%L >= %L", left.toTemplate(), right.toTemplate()
        )
    } else {
        CodeBlock.of(
            "%L > %L", left.toTemplate(), right.toTemplate()
        )
    }

    is KobolIRTree.Expression.BooleanExpression.BooleanLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.BooleanExpression.Eq -> CodeBlock.of(
        "%L == %L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Not -> CodeBlock.of(
        "!(%L)", condition.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.NotEq -> CodeBlock.of(
        "%L != %L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Or -> CodeBlock.of(
        "%L || %L", left.toTemplate(), right.toTemplate()
    )

    is KobolIRTree.Expression.BooleanExpression.Smaller -> if (equals) {
        CodeBlock.of(
            "%L <= %L", left.toTemplate(), right.toTemplate()
        )
    } else {
        CodeBlock.of(
            "%L < %L", left.toTemplate(), right.toTemplate()
        )
    }
}

private fun KobolIRTree.Expression.NumberExpression.toTemplate(escape: Boolean = false): CodeBlock = when (this) {
    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.NumberExpression.NumberVariable -> toCodeBlock(escape)
}

private fun TypeSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addMethod(data.toJava())
        is KobolIRTree.Types.Type.Class -> addClass(data)
        is KobolIRTree.Types.Type.External -> TODO()
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun TypeSpec.Builder.addClass(data: KobolIRTree.Types.Type.Class) {
    val classBuilder = if (data.isObject) {
        TypeSpec.objectBuilder(data.name)
    } else {
        TypeSpec.classBuilder(data.name)
    }
    classBuilder.addKdoc(data.doc.joinToString("\n"))

    if (data.init.isNotEmpty()) {
        val initBlock = CodeBlock.builder()
        for (init in data.init) {
            initBlock.add(init.toKotlin())
        }
        classBuilder.addInitializerBlock(initBlock.build())
    }

    for (member in data.members) {
        classBuilder.addProperty(member.createProperty())
    }

    for (function in data.functions) {
        classBuilder.addFunction(function.toKotlin())
    }

    addType(classBuilder.build())
}

private fun TypeSpec.Builder.addGlobalVariable(data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration

    addField(
        declaration.createProperty().toBuilder().apply {
            if (data.const) {
                addModifiers(Modifier.FINAL)
            }
        }.build()
    )
}

private fun Declaration.createProperty(): FieldSpec {
    val (type, init) = when (this) {
        is StringDeclaration -> {
            val value = value
            if (value != null) {
                TypeName(String::class) to value.toTemplate()
            } else STRING.copy(nullable = true) to null
        }

        is BooleanDeclaration -> {
            val value = value
            if (value != null) {
                BOOLEAN to value.toTemplate()
            } else BOOLEAN.copy(nullable = true) to null
        }

        is DoubleDeclaration -> {
            val value = value
            if (value != null) {
                DOUBLE to value.toTemplate()
            } else DOUBLE.copy(nullable = true) to null
        }

        is IntDeclaration -> {
            val value = value
            if (value != null) {
                INT to value.toTemplate()
            } else INT.copy(nullable = true) to null
        }
    }
    return PropertySpec.builder(
        name = name, type = type
    ).apply {
        mutable(mutable)
        if (private) {
            addModifiers(KModifier.PRIVATE)
        }
        initializer(init ?: CodeBlock.of("null"))
        addKdoc(comments.joinToString(separator = "\n"))
    }.build()
}

private val Declaration.Type: TypeName
    get() = when (this) {
        is StringDeclaration -> ClassName.get("java.lang", "String")
        is BooleanDeclaration -> TypeName.BOOLEAN
        is DoubleDeclaration -> TypeName.DOUBLE
        is IntDeclaration -> TypeName.INT
    }

fun generate(file: File, output: File, optimize: Boolean) {
    generate(setOf(file), output, optimize)
}

fun generate(files: Set<File>, output: File, optimize: Boolean) {
    for (ir in files.toIR()) {
        val finished = if (optimize) {
            ir.optimize()
        } else ir

        val java = generate(finished)
        java.writeTo(File(output, "java"))
    }
}
