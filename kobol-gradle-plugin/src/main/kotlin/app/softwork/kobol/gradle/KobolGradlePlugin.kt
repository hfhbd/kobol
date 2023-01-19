package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.plugins.JavaPlugin.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(project: Project) {
        val cobols = project.objects.domainObjectContainer(CobolSource::class.java)
        project.extensions.add("cobol", cobols)

        val convertAll = project.tasks.register("convertCobol")

        val cobolFiles = project.layout.projectDirectory.asFileTree.matching {
            include {
                it.name.endsWith(".cbl")
            }
        }.elements.map {
            it.map { file: FileSystemLocation ->
                val cobolSource = project.objects.newInstance(
                    CobolSource::class.java,
                    file.asFile.nameWithoutExtension,
                    file
                )
                cobolSource.apply {
                    convertAll.configure { dependsOn(convert) }
                }
                cobolSource
            }
        }
        cobols.addAll(cobolFiles.get())


        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            project.tasks.named("compileKotlin") { dependsOn(convertAll) }
        }

        project.plugins.withId("org.gradle.java") {
            project.tasks.named(COMPILE_JAVA_TASK_NAME) { dependsOn(convertAll) }
        }

        val upload = project.tasks.register("uploadCobol", UploadTask::class.java)
        val buildCobol = project.tasks.register("buildCobol", BuildTask::class.java) {
            dependsOn(upload)
        }
        project.tasks.register("runCobol", KobolRunTask::class.java) {
            dependsOn(buildCobol)
        }

        project.tasks.register("flowGraph", KobolFlowGraph::class.java)
        project.tasks.register("cleanCobol", CleanCobol::class.java) {
            uploaded.convention(upload.flatMap { it.uploaded })
        }
    }
}
