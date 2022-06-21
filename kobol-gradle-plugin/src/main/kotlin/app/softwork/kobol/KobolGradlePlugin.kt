package app.softwork.kobol

import app.softwork.kobol.generator.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import java.io.*
import javax.inject.*

class KobolGradlePlugin: Plugin<Project> {
    override fun apply(target: Project) {
        target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
    }
}

@CacheableTask
abstract class KobolTask: SourceTask() {
    init {
        group = "Kobol"
    }

    @InputFiles
    @SkipWhenEmpty
    @IgnoreEmptyDirectories
    @PathSensitive(PathSensitivity.RELATIVE)
    override fun getSource(): FileTree {
        return super.getSource()
    }

    @get:OutputDirectory
    var outputFolder: File? = null

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation().submit(ExecuteKobol::class.java) {
            it.inputFile.set(source.singleFile)
            it.outputFolder.set(outputFolder)
        }
    }
}


abstract class ExecuteKobol: WorkAction<ExecuteKobol.Parameters> {
    interface Parameters: WorkParameters {
        val inputFile: RegularFileProperty
        val outputFolder: DirectoryProperty
    }

    override fun execute() {
        val input = parameters.inputFile.get().asFile
        val outputFolder = parameters.outputFolder.get().asFile
        KotlinGenerator.generate(input, outputFolder)
    }
}
