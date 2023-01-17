package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.work.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFlowGraph : DefaultTask() {
    init {
        group = "Kobol"
    }

    @get:Input
    public abstract val sources: SetProperty<CobolSource>

    @get:InputFiles
    @get:PathSensitive(RELATIVE)
    public abstract val plugins: ConfigurableFileCollection

    init {
        plugins.setFrom(sources.flatMap {
            val pluginsConfigs = project.objects.listProperty(Configuration::class.java)
            for (it in it) {
                pluginsConfigs.add(it.plugins)
            }
            pluginsConfigs
        })
    }

    @get:OutputDirectory
    public abstract val outputFolder: DirectoryProperty

    init {
        outputFolder.convention(project.layout.buildDirectory.dir("reports/kobol/flowGraph"))
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generateFlow() {
        workerExecutor.classLoaderIsolation {
            it.classpath.from(plugins)
        }.submit(CreateFlowGraph::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
        }
    }
}
