package app.softwork.kobol.compiler

import app.softwork.kobol.*
import com.squareup.kotlinpoet.*
import java.io.*

object KobolCompiler {
    fun generateMain(file: CobolFile) = file.main {
        val procedure = file.procedure()

        procedure.displays.displayList.forEach {
            addStatement("println(%S)", it.string.text.drop(1).dropLast(1))
        }
    }

    private fun CobolFile.main(mainFunction: FunSpec.Builder.() -> Unit): FileSpec {
        val name = getProgramName().lowercase()
        return FileSpec.builder(name, name)
            .addFunction(
                FunSpec.builder("main").apply(mainFunction)
                    .build()
            )
            .build()
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
    return children
        .filterIsInstance<CobolLine>()
        .mapNotNull {
            it.exp?.procedure
        }.single()
}
