package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolTask: DefaultTask() {
    init {
        group = "Kobol"
    }

    @get:InputFiles
    @get:SkipWhenEmpty
    @get:IgnoreEmptyDirectories
    @get:PathSensitive(PathSensitivity.RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:OutputDirectory
    public abstract val outputFolder: DirectoryProperty

    @get:Optional
    @get:OutputDirectory
    public abstract val sqlFolder: DirectoryProperty

    @get:Input
    public abstract val optimize: Property<Boolean>

    init {
        optimize.convention(false)
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol"))

        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
            val kotlin = srcSet.extensions.getByName("kotlin") as SourceDirectorySet
            kotlin.srcDir(outputFolder.dir("kotlin"))
        }
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation().submit(ExecuteKobol::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
            it.optimize.set(optimize)
            it.sqlFolder.set(sqlFolder)
        }
    }
}
