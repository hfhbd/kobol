package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class JavaKobolTask: SourceTask() {
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
    public abstract val outputFolder: DirectoryProperty

    @get:Input
    public abstract val optimize: Property<Boolean>

    @get:Input
    public abstract val java8: Property<Boolean>

    init {
        optimize.convention(false)
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol"))
        java8.convention(true)

        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
            srcSet.java.srcDir(outputFolder.dir("java"))
        }
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation().submit(ExecuteJavaKobol::class.java) {
            it.inputFiles.setFrom(source)
            it.outputFolder.set(outputFolder)
            it.optimize.set(optimize)
            it.java8.set(java8)
        }
    }
}
