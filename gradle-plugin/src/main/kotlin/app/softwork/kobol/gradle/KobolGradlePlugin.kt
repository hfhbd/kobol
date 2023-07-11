package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(project: Project): Unit = project.run {
        val kobolClasspath = project.configurations.register("kobol") {
            it.dependencies.add(project.dependencies.create("app.softwork.kobol:ir:$KOBOL_VERSION"))
            it.dependencies.add(project.dependencies.create("app.softwork.kobol:intellij-env:$KOBOL_VERSION"))
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
            it.isVisible = true
        }

        val upload = registerHelperTasks(tasks)

        pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
            extensions.getByType(SourceSetContainer::class.java).named("main") {
                val convert = project.createConvertTask(it, kobolClasspath, upload)

                val kotlin = it.extensions.getByName("kotlin") as SourceDirectorySet
                kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
            }
        }

        pluginManager.withPlugin("org.gradle.java") {
            extensions.getByType(SourceSetContainer::class.java).named("main") {
                val convert = project.createConvertTask(
                    it,
                    kobolClasspath,
                    upload
                )
                it.java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
            }
        }
    }

    private fun Project.createConvertTask(
        sourceSet: SourceSet,
        classpath: Provider<out FileCollection>,
        uploadTask: TaskProvider<out UploadTask>
    ): TaskProvider<out KobolTask> {
        val taskName = "convertCobol"
        if (taskName in tasks.names) return tasks.named(taskName, KobolTask::class.java)

        val convert = tasks.register(taskName, KobolTask::class.java)

        val cobolSrc = objects.sourceDirectorySet("cobol", "cobol")
        cobolSrc.filter.include("*.cbl")
        cobolSrc.srcDir(file("src/${sourceSet.name}/cobol"))
        cobolSrc.destinationDirectory.convention(layout.buildDirectory.dir("generated/kobol/"))

        convert.configure {
            it.classpath.from(classpath)
            it.sources.from(cobolSrc.sourceDirectories)
            it.outputFolder.convention(cobolSrc.destinationDirectory)
        }

        cobolSrc.compiledBy(convert, KobolTask::outputFolder)
        sourceSet.extensions.add("cobol", cobolSrc)

        uploadTask.configure { 
            it.files.from(cobolSrc)
        }
        
        return convert
    }

    private fun registerHelperTasks(tasks: TaskContainer): TaskProvider<out UploadTask> {
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
        return upload
    }
}
