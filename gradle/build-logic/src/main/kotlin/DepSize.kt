import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import java.io.*

@CacheableTask
abstract class DepSize : DefaultTask() {
    @get:InputFiles
    @get:Classpath
    abstract val configurationToCheck: ConfigurableFileCollection

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    init {
        outputDirectory.convention(project.layout.buildDirectory.dir("reports/size"))
    }

    @TaskAction
    fun measure() {
        val sum = configurationToCheck.sumOf { it.length() } / 1024
        File(outputDirectory.asFile.get(), "size.txt").apply {
            if (exists()) {
                createNewFile()
            }
        }.writeText(
            configurationToCheck.sortedByDescending {
                it.length()
            }.joinToString(
                prefix = "$sum kb\n",
                postfix = "\n",
                separator = "\n",
            ) {
                "${it.name.padEnd(65)} ${(it.length() / 1024).toString().padStart(10)} kb"
            },
        )
    }
}
