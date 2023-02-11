import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import java.io.*

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
    
    @get:Input
    abstract val fileName: Property<String>

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    init {
        outputDirectory.convention(project.layout.buildDirectory.dir("customRepo"))
    }

    @TaskAction
    fun write() {
        File(outputDirectory.get().asFile, "updatePlugins.xml").writeText(
            """
                <plugins>
                <plugin id="app.softwork.kobol" url="https://hfhbd.github.io/kobol/${fileName.get()}-${version.get()}.zip" version="${version.get()}">
                <idea-version since-build="${sinceBuild.get()}" until-build="${untilBuild.get()}"/>
                </plugin>
                </plugins>
            """.trimIndent()
        )
    }
}
