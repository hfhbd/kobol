package app.softwork.kobol.generator.java

import app.softwork.kobol.fir.notPossible
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import com.squareup.javapoet.*
import javax.lang.model.element.Modifier.*

internal fun generateJava(tree: KobolIRTree): List<JavaFile> {
    val external = tree.types.mapNotNull {
        if (it is KobolIRTree.Types.Type.Class && it.isObject) {
            it
        } else null
    }
    val klass = TypeSpec.classBuilder(tree.name.replaceFirstChar { it.uppercaseChar() }).apply {
        addModifiers(PUBLIC)
    }
    
    klass.addType(tree.main)
    
    tree.types.forEach {
        if (it !in external) {
            klass.addType(it)
        }
    }
    val fileSpec = JavaFile.builder(tree.name, klass.build())
    fileSpec.skipJavaLangImports(true)

    val packageName = if (tree.packageName != null) {
        "${tree.packageName}.${tree.name}"
    } else tree.name
    val externalTypes = external.map {
        JavaFile.builder(packageName, it.toJava()).skipJavaLangImports(true)
    }

    return (externalTypes + fileSpec).map { it.build() }
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
        returns(this@toJava.returnType.Type)
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

private fun CodeBlock.Builder.addComment(format: String): CodeBlock.Builder = apply {
    add("// $format\n")
}

private fun KobolIRTree.Types.Function.Statement.dec(): CodeBlock {
    val declaration = this
    return if (declaration is Declaration) {
        declaration.member()
    } else declaration.toJava()
}

private fun KobolIRTree.Types.Function.Statement.toJava(): CodeBlock = CodeBlock.builder().let { code ->
    if (this !is Declaration && comments.isNotEmpty()) {
        for (comment in comments) {
            code.addComment(comment)
        }
    }
    when (this) {
        is Assignment -> code.add("\$L = \$L;\n", declaration.dec(), newValue.toTemplate())
        is Math -> {
            val op = when (op) {
                Math.Operation.Add -> "+="
                Math.Operation.Sub -> "-="
            }
            code.add("\$L $op \$L;\n", declaration.dec(), value.toTemplate())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> {
            val target = target.toTemplate()
            val variable = variable.toTemplate()
            code.add("\$L.\$L", target, variable)
        }

        is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> {
            val target = target.toTemplate()
            val variable = variable.toTemplate()
            code.add("\$L.\$L", target, variable)
        }

        is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> {
            val target = target.toTemplate()
            val variable = variable.toTemplate()
            code.add("\$L.\$L", target, variable)
        }

        is Print -> code.println(this)
        is Declaration -> code.add(CodeBlock.of("\$L", createProperty()))
        is FunctionCall -> code.add(call())
        is Use -> code.add(target.toJava()).add(".").add(action.toJava()).build()
        is Static -> code.add("\$T", ClassName.get(type.packageName, type.name))
        is Exit -> code.add("System.exit(\$L);\n", returnVariable.toTemplate())
        is Throw -> code.add("throw \$L;\n", expr.toTemplate())
        is Return -> code.add("return \$L;\n", expr.toTemplate())
        is LoadExternal -> code.add("System.loadLibrary(\"$libName\");\n")
        is DoWhile -> {
            code.beginControlFlow("do")
            for (statement in statements) {
                code.add(statement.toJava())
            }
            code.unindent()
            code.add("} while (\$L);\n", condition.toTemplate())
        }

        is For -> {
            val init = CodeBlock.of("\$L = \$L", counter.name, from.toTemplate())
            val condition = condition.toTemplate()
            val step = step
            val stepBlock = if (step != null) {
                CodeBlock.of("\$L += \$L", counter.name, step.toTemplate())
            } else CodeBlock.of("\$L++", counter.name)

            code.beginControlFlow("for (\$L; \$L; \$L)", init, condition, stepBlock)
            for (stmt in statements) {
                code.add(stmt.toJava())
            }
            code.endControlFlow()
        }

        is ForEach -> {
            code.beginControlFlow("for (\$N: \$L)", variable.name, provider.toTemplate())
            for (stmt in statements) {
                code.add(stmt.toJava())
            }
            code.endControlFlow()
        }

        is While -> {
            code.beginControlFlow("while (\$L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toJava()
                code.add(add)
            }
            code.endControlFlow()
        }

        is If -> {
            code.beginControlFlow("if (\$L)", condition.toTemplate())
            for (stmt in statements) {
                val add = stmt.toJava()
                code.add(add)
            }
            if (elseStatements.isNotEmpty()) {
                code.nextControlFlow("else")
                for (stmt in elseStatements) {
                    val add = stmt.toJava()
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
                    code.add("\$L", stmt.toJava())
                }
                code.endControlFlow()
            }
            val elseCase = elseCase
            if (elseCase != null) {
                code.beginControlFlow("else ->")
                for (stmt in elseCase.action) {
                    code.add("\$L", stmt.toJava())
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
            add("System.out.println(\$L);\n", expr.toTemplate())
        }

        is KobolIRTree.Expression.StringExpression.StringVariable -> {
            add("System.out.println(\$L);\n", expr.target.member())
        }

        is KobolIRTree.Expression.StringExpression.Concat -> {
            val template = expr.toTemplate()
            add("System.out.println(\$L);\n", template)
        }

        is KobolIRTree.Expression.StringExpression.Interpolation -> {
            val template = expr.expr.toTemplate()
            add("System.out.println(\$L);\n", template)
        }

        is KobolIRTree.Expression.StringExpression.StringVariable.Use -> {
            val target = expr.target.toTemplate()
            val variable = expr.variable.toTemplate()
            add("System.out.println(\$L.\$L);\n", target, variable)
        }
    }
}

private fun FunctionCall.call() = CodeBlock.builder().apply {
    val params = CodeBlock.builder().apply {
        for ((index, parameter) in parameters.withIndex()) {
            val toAdd = parameter.toTemplate()
            if (index == parameters.lastIndex) {
                add(toAdd)
            } else {
                add("$toAdd, ")
            }
        }
    }.build()

    when (val function = function) {
        is KobolIRTree.Types.Function -> {
            val method = MethodSpec.methodBuilder(function.name).build()
            if (function.external) {
                add("\$N.invoke(\$L);\n", method, params)
            } else {
                add("\$N(\$L);\n", method, params)
            }
        }

        is KobolIRTree.Types.Type.Class -> {
            add("new \$T(\$L);\n", ClassName.get(function.packageName, function.name), params)
        }
    }

}.build()

private fun KobolIRTree.Expression.toTemplate(): CodeBlock = when (this) {
    is KobolIRTree.Expression.StringExpression -> toTemplate()
    is KobolIRTree.Expression.NumberExpression -> toTemplate()
    is KobolIRTree.Expression.BooleanExpression -> toTemplate()
    is FunctionCall -> call()
    is Use -> CodeBlock.builder().add(target.toJava()).add(".").add(action.toJava()).build()
    is If -> TODO()
    is When -> TODO()
    is KobolIRTree.Types.Type.GlobalVariable -> toCodeBlock()
    is KobolIRTree.Expression.ObjectVariable -> toCodeBlock()
}

private fun Declaration.member(): CodeBlock = CodeBlock.of(name)

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
    is KobolIRTree.Expression.StringExpression.StringVariable.Use -> CodeBlock.of(
        "\$L.\$L",
        target.toTemplate(),
        variable.toTemplate()
    )
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
    is KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use -> CodeBlock.of(
        "\$L.\$L",
        target.toTemplate(),
        variable.toTemplate()
    )

    is KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use -> CodeBlock.of(
        "\$L.\$L",
        target.toTemplate(),
        variable.toTemplate()
    )
}

private fun TypeSpec.Builder.addType(data: KobolIRTree.Types) {
    when (data) {
        is KobolIRTree.Types.Function -> addMethod(data.toJava())
        is KobolIRTree.Types.Type.Class -> addType(data.toJava())
        is KobolIRTree.Types.Type.GlobalVariable -> {
            addGlobalVariable(data)
        }

        KobolIRTree.Types.Type.Void -> Unit
    }
}

private fun KobolIRTree.Types.Type.Class.toJava2(): TypeSpec = toJava()

private fun KobolIRTree.Types.Type.Class.toJava(): TypeSpec = TypeSpec.classBuilder(name).apply {
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

    for (inner in inner) {
        addType(inner.toJava2())
    }
}.build()

private fun TypeSpec.Builder.addGlobalVariable(data: KobolIRTree.Types.Type.GlobalVariable) {
    val declaration = data.declaration
    addField(declaration.createProperty())
}

private fun Declaration.createProperty(): FieldSpec {
    val (type, init) = when (this) {
        is StringDeclaration -> Type to value?.toTemplate()
        is BooleanDeclaration, is DoubleDeclaration, is IntDeclaration -> {
            if (value != null) {
                Type to value?.toTemplate()
            } else {
                Type.box() to null
            }
        }

        is ObjectDeclaration -> Type to value?.toTemplate()
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
        is ObjectDeclaration -> ClassName.get(type.packageName, type.name)
    }

private val KobolIRTree.Types.Type: TypeName
    get() = when (this) {
        is KobolIRTree.Types.Function -> notPossible()
        is KobolIRTree.Types.Type.Class -> ClassName.get(packageName, name)
        is KobolIRTree.Types.Type.GlobalVariable -> declaration.Type
        KobolIRTree.Types.Type.Void -> TypeName.VOID
    }
