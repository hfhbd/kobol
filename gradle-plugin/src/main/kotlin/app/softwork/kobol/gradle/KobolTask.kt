package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolTask
@Inject constructor(
    cobolName: String
) : DefaultTask() {
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
        outputFolder.convention(project.layout.buildDirectory.dir("generated/kobol/$cobolName"))
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generate() {
        workerExecutor.classLoaderIsolation {
            it.classpath.from(classpath)
        }.submit(ExecuteKobol::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
            it.sqlFolder.set(sqlFolder)
            it.config.set(pluginConfiguration)
        }
    }
}
