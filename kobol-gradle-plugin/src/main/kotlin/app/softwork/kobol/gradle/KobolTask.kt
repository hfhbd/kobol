package app.softwork.kobol.gradle

import app.softwork.kobol.ir.*
import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolTask : DefaultTask() {
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

    @get:InputFiles
    @get:PathSensitive(PathSensitivity.RELATIVE)
    internal abstract val classpath: ConfigurableFileCollection

    @get:Input
    public abstract val pluginConfiguration: MapProperty<String, Map<String, String>>

    init {
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol"))

        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
            val kotlin = srcSet.extensions.getByName("kotlin") as SourceDirectorySet
            kotlin.srcDir(outputFolder.dir("kotlin"))
        }
        project.plugins.withId("org.gradle.java") {
            val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
            srcSet.java.srcDir(outputFolder.dir("java"))
        }
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        println(classpath.toList())
        val s = workerExecutor.classLoaderIsolation {
            classpath.from(this@KobolTask.classpath)
        }

        s.submit(ExecuteKobol::class.java) {
            inputFiles.setFrom(sources)
            outputFolder.set(this@KobolTask.outputFolder)
            sqlFolder.set(this@KobolTask.sqlFolder)
            config.set(pluginConfiguration)
        }
    }
}
