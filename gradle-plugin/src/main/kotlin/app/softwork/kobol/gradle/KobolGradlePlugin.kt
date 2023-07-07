import app.softwork.kobol.gradle.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(project: Project): Unit = project.run {
        val cobols = objects.domainObjectContainer(CobolSource::class.java)
        extensions.add("cobol", cobols)

        val convertAll = tasks.register("convertCobol") {
            it.group = "Kobol"
        }

        val cobolFiles = layout.projectDirectory.asFileTree.matching {
            it.include {
                !it.isDirectory && it.name.endsWith(".cbl")
            }
        }.elements.map { cobolFiles ->
            cobolFiles.map { cobolFile ->
                val name = cobolFile.asFile.nameWithoutExtension
                val cobolSource = objects.newInstance(
                    CobolSource::class.java,
                    name,
                    cobolFile
                )
                val convert = tasks.register(cobolSource.taskName, KobolTask::class.java, name)
                convert.configure {
                    it.classpath.from(configurations.named(cobolSource.plugins))
                    it.sources.from(cobolFiles)
                }

                pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
                    extensions.getByType(SourceSetContainer::class.java).named("main") {
                        val kotlin = it.extensions.getByName("kotlin") as SourceDirectorySet
                        kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
                    }
                }
                pluginManager.withPlugin("org.gradle.java") {
                    extensions.getByType(SourceSetContainer::class.java).named("main") {
                        it.java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
                    }
                }

                convertAll.configure {
                    it.dependsOn(convert)
                }
                cobolSource
            }
        }
// https://github.com/gradle/gradle/issues/23540
        cobols.addAll(cobolFiles.get())

        val upload = tasks.register("uploadCobol", UploadTask::class.java)
        val buildCobol = tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
        }
        tasks.register("runCobol", KobolRunTask::class.java) {
            it.dependsOn(buildCobol)
        }

        tasks.register("cleanCobol", CleanCobol::class.java) {
            it.uploaded.convention(upload.flatMap { it.uploaded })
        }
    }
}
