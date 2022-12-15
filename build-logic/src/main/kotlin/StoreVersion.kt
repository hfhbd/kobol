import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import java.io.*

@CacheableTask
abstract class StoreVersion : DefaultTask() {
    @get:Input
    abstract val version: Property<String>

    init {
        version.convention(project.version.toString())
    }

    @get:OutputDirectory
    abstract val generated: DirectoryProperty

    init {
        generated.convention(project.layout.buildDirectory.dir("generated/kobol"))
    }

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    init {
        outputDirectory.convention(generated.dir("app/softwork/kobol/gradle"))
    }

    @TaskAction
    fun action() {
        File(outputDirectory.get().asFile, "Version.kt").writeText(
            """
            |package app.softwork.kobol.gradle
            |
            |internal val kobolVersion = "${version.get()}"
            |""".trimMargin()
        )
    }
}
