package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class JavaKobolTask: DefaultTask() {
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

    @get:Input
    public abstract val optimize: Property<Boolean>

    @get:Input
    public abstract val java8: Property<Boolean>

    @get:InputFiles
    @get:PathSensitive(PathSensitivity.RELATIVE)
    internal abstract val classpath: ConfigurableFileCollection

    @get:Input
    public abstract val pluginConfiguration: MapProperty<String, Map<String, String>>

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
        workerExecutor.classLoaderIsolation {
            it.classpath.setFrom(classpath)
        }.submit(ExecuteJavaKobol::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
            it.optimize.set(optimize)
            it.java8.set(java8)
            it.config.set(pluginConfiguration)
        }
    }
}
