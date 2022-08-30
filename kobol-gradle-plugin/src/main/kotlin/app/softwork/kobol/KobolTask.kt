package app.softwork.kobol

import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import org.jetbrains.kotlin.gradle.dsl.*
import javax.inject.*

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
    abstract val outputFolder: DirectoryProperty

    @get:Input
    abstract val optimize: Property<Boolean>

    init {
        optimize.convention(false)
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol"))

        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            val kotlin = project.extensions.getByName("kotlin") as KotlinProjectExtension
            kotlin.sourceSets.getByName("main").kotlin.srcDir(outputFolder.dir("kotlin"))
        }
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation().submit(ExecuteKobol::class.java) {
            it.inputFiles.setFrom(source)
            it.outputFolder.set(outputFolder)
            it.optimize.set(optimize)
        }
    }
}
