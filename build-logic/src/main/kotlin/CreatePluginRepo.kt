import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*

@CacheableTask
abstract class CreatePluginRepo : DefaultTask() {
    @get:Input
    abstract val version: Property<String>

    init {
        version.convention(project.version.toString())
    }

    @get:Input
    abstract val sinceBuild: Property<String>

    @get:Input
    abstract val untilBuild: Property<String>

    @get:OutputFile
    abstract val outputFile: RegularFileProperty

    init {
        outputFile.convention(project.layout.buildDirectory.file("customRepo/updatePlugins.xml"))
    }

    @TaskAction
    fun write() {
        val xml = outputFile.get().asFile
        xml.writeText(
            """
                <plugins>
                <plugin id="app.softwork.kobol" url="https://hfhbd.github.io/kobol/kobol-intellij-plugin-${version.get()}.zip" version="${version.get()}">
                <idea-version since-build="${sinceBuild.get()}" until-build="${untilBuild.get()}"/>
                </plugin>
                </plugins>
            """.trimIndent()
        )
    }
}
