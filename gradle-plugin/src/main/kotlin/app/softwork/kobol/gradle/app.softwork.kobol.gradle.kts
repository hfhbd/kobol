package app.softwork.kobol.gradle

import org.gradle.api.plugins.JavaPlugin.*

private val cobols = objects.domainObjectContainer(CobolSource::class.java)
extensions.add("cobol", cobols)

private val convertAll = tasks.register("convertCobol") {
    group = "Kobol"
}

private val cobolFiles = layout.projectDirectory.asFileTree.matching {
    include {
        it.name.endsWith(".cbl")
    }
}.elements.map {
    it.map { file ->
        val cobolSource = objects.newInstance(
            CobolSource::class.java,
            file.asFile.nameWithoutExtension,
            file
        )
        val convert = tasks.register(cobolSource.taskName, KobolTask::class.java) {
            classpath.from(configurations.named(cobolSource.plugins))
            sources.from(file)
        }

        convertAll.configure {
            dependsOn(convert)
        }
        cobolSource
    }
}
cobols.addAll(cobolFiles.get())


plugins.withId("org.jetbrains.kotlin.jvm") {
    tasks.named("compileKotlin") { dependsOn(convertAll) }
}

plugins.withId("org.gradle.java") {
    tasks.named(COMPILE_JAVA_TASK_NAME) { dependsOn(convertAll) }
}

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
