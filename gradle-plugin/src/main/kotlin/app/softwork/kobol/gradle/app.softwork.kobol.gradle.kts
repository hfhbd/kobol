import app.softwork.kobol.gradle.*

private val cobols = objects.domainObjectContainer(CobolSource::class.java)
extensions.add("cobol", cobols)

private val convertAll = tasks.register("convertCobol") {
    group = "Kobol"
}

private val cobolFiles = layout.projectDirectory.asFileTree.matching {
    include {
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
        convert {
            classpath.from(configurations.named(cobolSource.plugins))
            sources.from(cobolFiles)
        }
        
        project.project.pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
            project.extensions.getByType(SourceSetContainer::class.java).named("main") {
                val kotlin = extensions.getByName("kotlin") as SourceDirectorySet
                kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
            }
        }
        project.project.pluginManager.withPlugin("org.gradle.java") {
            project.extensions.getByType(SourceSetContainer::class.java).named("main") {
                java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
            }
        }

        convertAll {
            dependsOn(convert)
        }
        cobolSource
    }
}
// https://github.com/gradle/gradle/issues/23540
cobols.addAll(cobolFiles.get())

private val upload = tasks.register("uploadCobol", UploadTask::class.java)
private val buildCobol = tasks.register("buildCobol", BuildTask::class.java) {
    dependsOn(upload)
}
tasks.register("runCobol", KobolRunTask::class.java) {
    dependsOn(buildCobol)
}

tasks.register("cleanCobol", CleanCobol::class.java) {
    uploaded.convention(upload.flatMap { it.uploaded })
}
