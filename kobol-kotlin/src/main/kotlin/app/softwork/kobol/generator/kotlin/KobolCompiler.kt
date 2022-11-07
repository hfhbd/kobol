package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.optimizations.*
import com.squareup.kotlinpoet.*
import java.io.*

fun generate(tree: KobolIRTree): FileSpec {
    val packageName = tree.name
    val fileSpec = FileSpec.builder(packageName, packageName).apply {
        tree.types.forEach {
            addType(packageName, it)
        }

        addFunction(tree.main.toKotlin(name))
    }
    return fileSpec.build()
}

private fun Declaration.toParameter() = ParameterSpec.builder(name, KType).apply {
    this@toParameter.annotations.forEach { (annotation, values) ->
        addAnnotation(
            AnnotationSpec.builder(ClassName.bestGuess(annotation)).apply {
                for (value in values) {
                    addMember("%L", CodeBlock.of(value))
                }
            }.build()
        )
    }
}.build()

private fun KobolIRTree.Types.Function.toKotlin(packageName: String): FunSpec {
    val name = if (external) "invoke" else name
    return FunSpec.builder(name).apply {
        if (private) {
            addModifiers(KModifier.PRIVATE)
        }
        this@toKotlin.parameters.forEach {
            addParameter(it.toParameter())
        }
        if (external) {
            addModifiers(KModifier.EXTERNAL, KModifier.OPERATOR)
        } else {
            for (it in body) {
                if (it is Declaration) {
                    addCode(it.toKotlin(packageName))
                } else {
                    addCode("%L\n", it.toKotlin(packageName))
                }
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

private fun KobolIRTree.Types.Function.Statement.toKotlin2(packageName: String): CodeBlock = toKotlin(packageName)

private fun KobolIRTree.Types.Function.Statement.toKotlin(packageName: String) = CodeBlock.builder().let { code ->
    if (this !is Declaration && comments.isNotEmpty()) {
        for (comment in comments) {
            code.addComment(comment)
        }
    }
    when (this) {
        is Use -> {
            val target = target
            val betterTarget = if (target is Declaration) {
                CodeBlock.of("%M", target.member(packageName))
            } else target.toKotlin2(packageName)
            val action = action
            val betterAction = if (action is Declaration) {
                CodeBlock.of("%M", action.member(packageName))
            } else action.toKotlin2(packageName)
            code.add(
                "%L.%L", betterTarget, betterAction
            )
        }

        is Static -> code.add("%T", ClassName(type.packageName, type.name))

        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> code.add("%L", toTemplate(packageName))
        is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> code.add(
            "%L", toTemplate(packageName)
        )

        is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> code.add(
            "%L", toTemplate(packageName)
        )

        is Assignment -> code.assign(this, packageName)
        is Print -> code.println(this, packageName)
        is Declaration -> code.add(CodeBlock.of("%L", createProperty(packageName)))
        is FunctionCall -> code.add("%L", call(packageName))

        is Exit -> code.add("return %M(0)", MemberName("kotlin.system", "exitProcess", true))
        is LoadExternal -> code.add("System.loadLibrary(\"$libName\")")
        is DoWhile -> code.beginControlFlow("do").addStatement("%L", functionCall.call(packageName)).unindent()
            .add("} while (%L)", condition.toTemplate(packageName))

        is For -> {
            code.add("%L = %L\n", counter.name, from.toTemplate(packageName))
            code.beginControlFlow("while (%L)", condition.toTemplate(packageName))
            for (stmt in statements) {
                code.addStatement("%L", stmt.toKotlin2(packageName))
            }
            val step = step
            if (step != null) {
                code.addStatement("%L += %L", counter.name, step.toTemplate(packageName))
            }
            code.unindent()
            code.add("}")
        }

        is ForEach -> {
            code.beginControlFlow("for (%N in %L)", variable.name, provider.toTemplate(packageName))
            for (stmt in statements) {
                code.addStatement("%L", stmt.toKotlin2(packageName))
            }
            code.unindent()
            code.add("}")
        }

        is While -> {
            code.beginControlFlow("while (%L)", condition.toTemplate(packageName))
            for (stmt in statements) {
                val add = stmt.toKotlin2(packageName)
                code.addStatement("%L", add)
            }
            code.unindent()
            code.add("}")
        }

        is If -> {
            code.beginControlFlow("if (%L)", condition.toTemplate(packageName))
            for (stmt in statements) {
                val add = stmt.toKotlin2(packageName)
                code.add("%L", add)
                code.add("\n")
            }

            for (elseIf in elseIfs) {
                code.nextControlFlow("else if (%L)", elseIf.condition.toTemplate(packageName))
                for (stmt in elseIf.statements) {
                    val add = stmt.toKotlin2(packageName)
                    code.addStatement("%L", add)
                }
            }

            if (elseStatements.isNotEmpty()) {
                code.nextControlFlow("else")
                for (stmt in elseStatements) {
                    val add = stmt.toKotlin2(packageName)
                    code.addStatement("%L", add)
                }
            }
            code.unindent()
            code.add("}")
        }

        is When -> {
            when (this) {
                is When.Single -> code.beginControlFlow("when (%L)", expr.toTemplate(packageName))
                is When.Multiple -> code.beginControlFlow("when")
            }

            for (case in cases) {
                when (case) {
                    is When.Single.Case -> code.beginControlFlow("%L ->", case.condition.toTemplate(packageName))
                    is When.Multiple.Case -> code.beginControlFlow("%L ->", case.condition.toTemplate(packageName))
                }

                for (stmt in case.action) {
                    code.addStatement("%L", stmt.toKotlin2(packageName))
                }
                code.endControlFlow()
            }
            val elseCase = elseCase
            if (elseCase != null) {
                code.beginControlFlow("else ->")
                for (stmt in elseCase.action) {
                    code.addStatement("%L", stmt.toKotlin2(packageName))
                }
                code.endControlFlow()
            }
            code.unindent()
            code.add("}")
        }
    }
    code.build()
}

private fun CodeBlock.Builder.assign(it: Assignment, packageName: String) {
    val dec = it.declaration
    val member = if (dec is Declaration) {
        CodeBlock.of("%M", dec.member(packageName))
    } else {
        dec.toKotlin(packageName)
    }
    add("%L = %L", member, it.newValue.toTemplate(packageName))
}

private fun CodeBlock.Builder.println(it: Print, packageName: String) {
    when (val expr = it.expr) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            add("println(%L)", expr.toTemplate(packageName, insideString = false))
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            add("println(%L)", expr.target.member(packageName))
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            val template = expr.toTemplate(packageName, insideString = true)
            add("println(\"%L\")", template)
        }

        is KobolIRTree.Expression.StringExpression.Interpolation -> {
            val template = expr.expr.toTemplate(packageName)
            add("println(%L)", template)
        }

        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> {
            add("println(%L)", expr.toTemplate(packageName))
        }
    }
}

private fun FunctionCall.call(packageName: String) = CodeBlock.builder().apply {
    val params = CodeBlock.builder().apply {
        for (parameter in parameters) {
            val member = parameter.toTemplate(packageName)
            if (parameter == parameters.last()) {
                add("%L", member)
            } else {
                add("%L, ", member)
            }
        }
    }.build()

    when (val function = function) {
        is KobolIRTree.Types.Function -> {
            val member = if (function.topLevel) {
                MemberName(function.packageName ?: "", function.name, isExtension = true)
            } else {
                MemberName(function.packageName ?: "", function.name)
            }
            add("%M(%L)", member, params)
        }

        is KobolIRTree.Types.Type.Class -> add(
            "%T(%L)", ClassName(function.packageName, function.name), params
        )
    }
}.build()

private fun KobolIRTree.Expression.toTemplate(packageName: String, insideString: Boolean = false): CodeBlock =
    when (this) {
        is KobolIRTree.Expression.StringExpression -> toTemplate(packageName, insideString)
        is KobolIRTree.Expression.NumberExpression -> toTemplate(packageName, insideString)
        is KobolIRTree.Expression.BooleanExpression -> toTemplate(packageName)
        is FunctionCall -> call(packageName)
        is If -> TODO()
        is When -> TODO()
        is KobolIRTree.Types.Type.GlobalVariable -> toCodeBlock(packageName, insideString)
        is Use -> toKotlin(packageName)

        is KobolIRTree.Expression.ObjectVariable -> toCodeBlock(packageName, insideString)
    }

private fun Declaration.member(packageName: String) = when (this) {
    is ObjectDeclaration -> {
        if (static) {
            MemberName(type.packageName, name)
        } else MemberName(packageName, name)
    }

    else -> MemberName(packageName, name)
}

private fun KobolIRTree.Expression.Variable.toCodeBlock(packageName: String, insideString: Boolean): CodeBlock {
    return if (insideString) {
        val memberName = target.member(packageName)
        CodeBlock.of("$%M", memberName)
    } else {
        val memberName = target.member(packageName)
        CodeBlock.of("%M", memberName)
    }
}

private fun KobolIRTree.Expression.StringExpression.toTemplate(packageName: String, insideString: Boolean): CodeBlock =
    when (this) {
        is KobolIRTree.Expression.StringExpression.StringLiteral -> {
            if (insideString) {
                CodeBlock.of("%L", value)
            } else {
                CodeBlock.of("%S", value)
            }
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            toCodeBlock(packageName, insideString)
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            CodeBlock.of(
                "%L%L",
                left.toTemplate(insideString = insideString, packageName = packageName),
                right.toTemplate(insideString = insideString, packageName = packageName)
            )
        }

        is KobolIRTree.Expression.StringExpression.Interpolation -> {
            expr.toTemplate(packageName, insideString)
        }

        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> CodeBlock.of(
            if (insideString) "\${%L.%L}" else "%L.%L",
            target.toTemplate(packageName, false),
            variable.toTemplate(packageName, false)
        )
    }

private fun KobolIRTree.Expression.BooleanExpression.toTemplate(packageName: String): CodeBlock = when (this) {
    is KobolIRTree.Expression.BooleanExpression.And -> CodeBlock.of(
        "%L && %L", left.toTemplate(packageName), right.toTemplate(packageName)
    )

    is KobolIRTree.Expression.BooleanExpression.Bigger -> if (equals) {
        CodeBlock.of(
            "%L >= %L", left.toTemplate(packageName), right.toTemplate(packageName)
        )
    } else {
        CodeBlock.of(
            "%L > %L", left.toTemplate(packageName), right.toTemplate(packageName)
        )
    }

    is KobolIRTree.Expression.BooleanExpression.BooleanLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.BooleanExpression.Eq -> CodeBlock.of(
        "%L == %L", left.toTemplate(packageName), right.toTemplate(packageName)
    )

    is KobolIRTree.Expression.BooleanExpression.Not -> CodeBlock.of(
        "!(%L)", condition.toTemplate(packageName)
    )

    is KobolIRTree.Expression.BooleanExpression.NotEq -> CodeBlock.of(
        "%L != %L", left.toTemplate(packageName), right.toTemplate(packageName)
    )

    is KobolIRTree.Expression.BooleanExpression.Or -> CodeBlock.of(
        "%L || %L", left.toTemplate(packageName), right.toTemplate(packageName)
    )

    is KobolIRTree.Expression.BooleanExpression.Smaller -> if (equals) {
        CodeBlock.of(
            "%L <= %L", left.toTemplate(packageName), right.toTemplate(packageName)
        )
    } else {
        CodeBlock.of(
            "%L < %L", left.toTemplate(packageName), right.toTemplate(packageName)
        )
    }
}

private fun KobolIRTree.Expression.NumberExpression.toTemplate(
    packageName: String, insideString: Boolean = false
): CodeBlock = when (this) {
    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral -> CodeBlock.of("%L", value)
    is KobolIRTree.Expression.NumberExpression.NumberVariable -> toCodeBlock(packageName, insideString)
    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> CodeBlock.of(
        if (insideString) "\${%L.%L}" else "%L.%L",
        target.toTemplate(packageName, false),
        variable.toTemplate(packageName, false)
    )

    is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> CodeBlock.of(
        if (insideString) "\${%L.%L}" else "%L.%L",
        target.toTemplate(packageName, false),
        variable.toTemplate(packageName, false)
    )
}

private fun FileSpec.Builder.addType(packageName: String, data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addFunction(data.toKotlin(packageName))
        is KobolIRTree.Types.Type.Class -> addClass(packageName, data)
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(packageName, data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun FileSpec.Builder.addClass(packageName: String, data: KobolIRTree.Types.Type.Class) {
    val classBuilder = if (data.isObject) {
        TypeSpec.objectBuilder(data.name)
    } else {
        TypeSpec.classBuilder(data.name)
    }

    if (data.isData) {
        classBuilder.modifiers.add(KModifier.DATA)
    }

    classBuilder.addKdoc(data.doc.joinToString("\n"))

    data.annotations.forEach { (annotation, values) ->
        classBuilder.addAnnotation(
            AnnotationSpec.builder(ClassName.bestGuess(annotation)).apply {
                for (value in values) {
                    addMember("%L", CodeBlock.of(value))
                }
            }.build()
        )
    }

    if (data.init.isNotEmpty()) {
        val initBlock = CodeBlock.builder()
        for (init in data.init) {
            initBlock.addStatement("%L", init.toKotlin(packageName))
        }
        classBuilder.addInitializerBlock(initBlock.build())
    }

    val const = data.constructor.takeIf { it.isNotEmpty() }
    if (const != null) {
        classBuilder.primaryConstructor(
            FunSpec.constructorBuilder().apply {
                for (const in const) {
                    addParameter(const.toParameter())
                }
            }.build()
        )
    }

    for (member in data.members) {
        classBuilder.addProperty(member.createProperty(packageName).let {
            if (data.isData) {
                it.toBuilder().initializer(member.name).apply {
                    annotations.clear()
                    mutable(false)
                }.build()
            } else it
        })
    }

    for (function in data.functions) {
        classBuilder.addFunction(function.toKotlin(packageName))
    }

    addType(classBuilder.build())
}

private fun FileSpec.Builder.addGlobalVariable(packageName: String, data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration

    addProperty(
        declaration.createProperty(packageName).toBuilder().apply {
            if (declaration is Primitive && declaration.const) {
                addModifiers(KModifier.CONST)
            }
        }.build()
    )
}

private fun Declaration.createProperty(packageName: String): PropertySpec {
    val init = when (this) {
        is StringDeclaration -> value?.toTemplate(packageName)
        is BooleanDeclaration -> value?.toTemplate(packageName)
        is DoubleDeclaration -> value?.toTemplate(packageName)
        is IntDeclaration -> value?.toTemplate(packageName)
        is ObjectDeclaration -> value?.toTemplate(packageName)
    }
    return PropertySpec.builder(
        name = name, type = KType.copy(nullable = nullable)
    ).apply {
        mutable(mutable)
        if (private) {
            addModifiers(KModifier.PRIVATE)
        }
        initializer(init ?: CodeBlock.of("null").takeIf { nullable })
        addKdoc(comments.joinToString(separator = "\n"))
        this@createProperty.annotations.forEach { (annotation, values) ->
            addAnnotation(
                AnnotationSpec.builder(ClassName.bestGuess(annotation)).apply {
                    for (value in values) {
                        addMember("%L", CodeBlock.of(value))
                    }
                }.build()
            )
        }
    }.build()
}

private val Declaration.KType: TypeName
    get() = when (this) {
        is StringDeclaration -> STRING
        is BooleanDeclaration -> BOOLEAN
        is DoubleDeclaration -> DOUBLE
        is IntDeclaration -> INT
        is ObjectDeclaration -> ClassName(type.packageName, type.name)
    }

fun generate(
    file: File,
    output: File,
    optimize: Boolean,
    fileHandling: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null
) {
    generate(setOf(file), output, optimize, fileHandling, serialization, sqlPrecompiler)
}

fun generate(
    files: Set<File>,
    output: File,
    optimize: Boolean,
    fileHandling: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null
) {
    for (ir in files.toIR(fileHandling, serialization, sqlPrecompiler)) {
        val finished = if (optimize) {
            ir.optimize()
        } else ir

        val kotlin = generate(finished)
        kotlin.writeTo(directory = File(output, "kotlin"))
    }
}
