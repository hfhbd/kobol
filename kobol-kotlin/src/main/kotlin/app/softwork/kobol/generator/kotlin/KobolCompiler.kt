package app.softwork.kobol.generator.kotlin

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

private fun KobolIRTree.Types.Function.toKotlin(): FunSpec {
    val name = if (external) "invoke" else name
    return FunSpec.builder(name).apply {
        if (private) {
            addModifiers(KModifier.PRIVATE)
        }
        this@toKotlin.parameters.forEach {
            addParameter(it.name, it.KType)
        }
        if (external) {
            addModifiers(KModifier.EXTERNAL, KModifier.OPERATOR)
        } else {
            body.forEach {
                addCode(it.toKotlin())
            }
            addKdoc(doc.joinToString(separator = "\n"))
            if (body.any { it is Exit }) {
                returns(NOTHING)
            }
        }
    }.build()
}

fun CodeBlock.Builder.addComment(format: String, vararg args: Any): CodeBlock.Builder = apply {
    add("//·${format.replace(' ', '·')}\n", *args)
}

@Suppress("NOTHING_TO_INLINE")
private inline fun KobolIRTree.Types.Function.Statement.toKotlin2(): CodeBlock = toKotlin()

private fun KobolIRTree.Types.Function.Statement.toKotlin() = CodeBlock.builder().let { code ->
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
        is FunctionCall.Fluent -> code.add(previous.toKotlin2()).add(".").add(action.call())

        is Exit -> code.addStatement("return %M(0)", MemberName("kotlin.system", "exitProcess", true))
        is LoadExternal -> code.addStatement("System.loadLibrary(\"$libName\")")
        is DoWhile -> code.beginControlFlow("do").add(functionCall.call()).unindent()
            .add("} while (%L)\n", condition.toTemplate())

        is ForEach -> {
            code.add("%L = %L\n", counter.name, from.toTemplate())
            code.beginControlFlow("while (%L)", condition.toTemplate())
            for (stmt in statements) {
                code.add(stmt.toKotlin2())
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
                val add = stmt.toKotlin2()
                code.add(add)
            }
            code.endControlFlow()
        }

        is If -> {
            code.beginControlFlow("if (%L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toKotlin2()
                code.add(add)
            }

            for (elseIf in elseIfs) {
                code.nextControlFlow("else if (%L)", elseIf.condition.toTemplate())
                for (stmt in elseIf.statements) {
                    val add = stmt.toKotlin2()
                    code.add(add)
                }
            }

            if (elseStatements.isNotEmpty()) {
                code.nextControlFlow("else")
                for (stmt in elseStatements) {
                    val add = stmt.toKotlin2()
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
                    code.add("%L", stmt.toKotlin2())
                }
                code.endControlFlow()
            }
            val elseCase = elseCase
            if (elseCase != null) {
                code.beginControlFlow("else ->")
                for (stmt in elseCase.action) {
                    code.add("%L", stmt.toKotlin2())
                }
                code.endControlFlow()
            }
            code.endControlFlow()
        }
    }
    code.build()
}

private fun CodeBlock.Builder.assign(it: Assignment) {
    val (member, klass) = it.declaration.member()
    if (klass != null) {
        addStatement("%N.%M = %L", klass, member, it.newValue.toTemplate())
    }
    else addStatement("%L = %L", member, it.newValue.toTemplate())
}

private fun CodeBlock.Builder.println(it: Print) {
    when (val expr = it.expr) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            addStatement("println(%L)", expr.toTemplate())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            addStatement("println(%L)", expr.target.member().first)
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
    val params = CodeBlock.builder().apply {
        for (parameter in parameters) {
            val className = parameter.className
            val member = if (className != null) {
                MemberName(ClassName("", className), parameter.name)
            } else {
                MemberName("", parameter.name)
            }
            if (parameter == parameters.last()) {
                add("%M", member)
            } else {
                add("%M, ", member)

            }
        }
    }.build()
    addStatement("%M(%L)", MemberName("", function.name), params)
}.build()

private fun KobolIRTree.Expression.toTemplate(escape: Boolean = true): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate(escape)
    is KobolIRTree.Expression.NumberExpression -> toTemplate(escape)
    is KobolIRTree.Expression.BooleanExpression -> toTemplate()
    is FunctionCall -> call()
    is FunctionCall.Fluent -> CodeBlock.builder().add(previous.toKotlin2()).add(".").add(action.call()).build()
    is DoWhile -> TODO()
    is ForEach -> TODO()
    is While -> TODO()
    is If -> TODO()
    is When -> TODO()
}

private fun Declaration.member(): Pair<MemberName, TypeSpec?> {
    val className = className
    val member = MemberName("", name)
    return member to if (className != null) {
        TypeSpec.classBuilder(className).build()
    } else null
}

private fun KobolIRTree.Expression.Variable.toCodeBlock(escape: Boolean): CodeBlock {
    return if (escape) {
        val (memberName, klass) = target.member()
        if (klass != null) {
            CodeBlock.of("%N.%L", klass, memberName)
        } else CodeBlock.of("%L", memberName)
    } else {
        val (memberName, klass) = target.member()
        if (klass != null) {
            CodeBlock.of("${"$"}{%N.%M}", klass, memberName)
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

private fun FileSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addFunction(data.toKotlin())
        is KobolIRTree.Types.Type.Class -> addClass(data)
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun FileSpec.Builder.addClass(data: KobolIRTree.Types.Type.Class) {
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

private fun FileSpec.Builder.addGlobalVariable(data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration

    addProperty(
        declaration.createProperty().toBuilder().apply {
            if (declaration is Primitive && declaration.const) {
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
                STRING to value.toTemplate()
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

        is ObjectDeclaration -> {
            val value = value
            if (value == null) {
                KType.copy(nullable = true) to null
            } else KType to value.toTemplate()
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

private val Declaration.KType: TypeName
    get() = when (this) {
        is StringDeclaration -> STRING
        is BooleanDeclaration -> BOOLEAN
        is DoubleDeclaration -> DOUBLE
        is IntDeclaration -> INT
        is ObjectDeclaration -> ClassName(type.packageName ?: "", type.name)
    }

fun generate(file: File, output: File, optimize: Boolean) {
    generate(setOf(file), output, optimize)
}

fun generate(files: Set<File>, output: File, optimize: Boolean) {
    for (ir in files.toIR()) {
        val finished = if (optimize) {
            ir.optimize()
        } else ir

        val kotlin = generate(finished)
        kotlin.writeTo(directory = File(output, "kotlin"))
    }
}
