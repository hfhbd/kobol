package app.softwork.kobol.gradle

import app.softwork.kobol.ir.*
import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.file.*
import org.gradle.api.internal.tasks.JvmConstants.COMPILE_JAVA_TASK_NAME
import org.gradle.api.provider.*
import org.gradle.api.provider.Provider
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
    @get:Classpath
    internal abstract val classpath: ConfigurableFileCollection

    @get:Input
    public abstract val pluginConfiguration: MapProperty<String, Map<String, String>>

    init {
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol"))
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation {
            classpath.from(this@KobolTask.classpath)
        }.submit(ExecuteKobol::class.java) {
            inputFiles.setFrom(sources)
            outputFolder.set(this@KobolTask.outputFolder)
            sqlFolder.set(this@KobolTask.sqlFolder)
            config.set(pluginConfiguration)
        }
    }
}

internal fun Project.configureTasks(kobolTask: Provider<KobolTask>) {
    project.plugins.withId("org.jetbrains.kotlin.jvm") {
        val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
        val kotlin = srcSet.extensions.getByName("kotlin") as SourceDirectorySet
        kotlin.srcDir(kobolTask.flatMap { it.outputFolder.dir("kotlin") })
        project.tasks.named("compileKotlin") {
            dependsOn(kobolTask)
        }
    }
    project.plugins.withId("org.gradle.java") {
        val srcSet = project.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
        srcSet.java.srcDir(kobolTask.flatMap { it.outputFolder.dir("java") })
        project.tasks.named(COMPILE_JAVA_TASK_NAME) {
            dependsOn(kobolTask)
        }
    }
}
