package app.softwork.kobol.compiler

import app.softwork.kobol.*
import com.squareup.kotlinpoet.*
import java.io.*

object KobolCompiler {
    fun generateMain(file: CobolFile) = file.main {
        val procedure = file.procedure()

        for (element in procedure.children) {
            when (element) {
                is CobolDisplay -> {
                    val string = element.string
                    if (string != null) {
                        addStatement("println(%S)", string.text.drop(1).dropLast(1))
                    } else {
                        addStatement("println(${element.varName!!.text!!})")
                    }
                }

                is CobolAssignment -> {
                    val name: String = element.varName.text
                    val valueElement = element.`var`.number ?: element.`var`.string
                    val value = valueElement!!.text
                    addStatement("val $name = $value")
                }
            }
        }
    }

    private fun CobolFile.main(mainFunction: FunSpec.Builder.() -> Unit): FileSpec {
        val name = getProgramName().lowercase()
        return FileSpec.builder(name, name).addFunction(
            FunSpec.builder("main").apply(mainFunction).build()
        ).build()
    }
}

fun KobolCompiler.generateMain(file: File): FileSpec {
    val env = CoreEnvironment(listOf(file)).apply {
        initializeApplication {
            registerFileType(CobolFileType, CobolFileType.defaultExtension)
            registerParserDefinition(CobolParserDefinition)
        }
    }
    var fileSpec: FileSpec? = null
    env.forSourceFiles<CobolFile> {
        fileSpec = generateMain(it)
    }
    return fileSpec!!
}

fun KobolCompiler.generateMain(file: File, output: File) {
    generateMain(file).writeTo(output)
}

fun CobolFile.getProgramName(): String {
    for (element in children) {
        if (element is CobolLine) {
            val programIDList: List<CobolProgramID>? = element.exp?.id?.programIDList?.takeIf { it.isNotEmpty() }
            if (programIDList != null) {
                return programIDList.single().varName.text
            }
        }
    }
    error("No PROGRAM-ID found")
}

fun CobolFile.procedure(): CobolProcedure {
    return children.filterIsInstance<CobolLine>().mapNotNull {
        it.exp?.procedure
    }.single()
}
