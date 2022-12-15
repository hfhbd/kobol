import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*

@CacheableTask
abstract class StoreVersion : DefaultTask() {
    @get:Input
    abstract val version: Property<String>

    init {
        version.convention(project.version.toString())
    }

    @get:PathSensitive(PathSensitivity.RELATIVE)
    @get:InputDirectory
    abstract val outputDirectory: DirectoryProperty

    init {
        outputDirectory.convention(project.layout.buildDirectory.dir("generated/kobol"))
    }

    @get:OutputFile
    abstract val outputFile: RegularFileProperty

    init {
        outputFile.convention(outputDirectory.file("app/softwork/kobol/gradle/Version.kt"))
    }

    @TaskAction
    fun action() {
        outputFile.get().asFile.writeText(
            """
            |package app.softwork.kobol.gradle
            |
            |internal val kobolVersion = "${version.get()}"
            |""".trimMargin()
        )
    }
}
