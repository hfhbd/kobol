package app.softwork.kobol.compiler

import app.softwork.kobol.*
import com.intellij.core.*
import com.intellij.openapi.util.*
import com.intellij.psi.*
import com.squareup.kotlinpoet.*
import java.io.*

object KobolCompiler {
    fun generateMain(file: CobolFile) = file.main {
        val displays = file.procedure()
        displays.displayList.forEach {
            addStatement("println(%S)", it.string.text)
        }
    }

    private fun CobolFile.main(mainFunction: FunSpec.Builder.() -> Unit): FileSpec {
        val name = getProgramName()
        return FileSpec.builder("", name)
            .addFunction(
                FunSpec.builder("main").apply(mainFunction)
                    .build()
            )
            .build()
    }
}

fun KobolCompiler.generateMain(file: File, output: File) {
    val env = CoreApplicationEnvironment(Disposer.newDisposable())
    val projectEnv = CoreProjectEnvironment(env.parentDisposable, env)
    PsiFileFactory.getInstance(projectEnv.project)
    env.registerFileType(CobolFileType, CobolFileType.defaultExtension)

    val virtualFile = projectEnv.environment.localFileSystem.findFileByIoFile(file) ?: error("File $file not found")

    val cobolFile = PsiManager.getInstance(projectEnv.project).findFile(virtualFile) as CobolFile
    val converted = generateMain(cobolFile)
    converted.writeTo(output)
}
