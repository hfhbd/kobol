import app.softwork.kobol.gradle.*

val kobol = extensions.create<Kobol>("kobol")

val kobolCompiler = configurations.dependencyScope("kobol") {
    fromDependencyCollector(kobol.dependencies.compiler)
}

kobol.dependencies {
    compiler("app.softwork.kobol:builder:$KOBOL_VERSION")
    compiler("app.softwork.kobol:intellij-env:$KOBOL_VERSION")
}

val kobolClasspath = configurations.resolvable("kobolClasspath") {
    // https://github.com/gradle/gradle/issues/26732
    extendsFrom(kobolCompiler.get())
}

val uploadCobol by tasks.registering(UploadTask::class)
val buildCobol by tasks.registering(BuildTask::class) {
    dependsOn(uploadCobol)
}
tasks.register("runCobol", KobolRunTask::class) {
    dependsOn(buildCobol)
}

tasks.register("cleanCobol", CleanCobol::class) {
    uploaded.convention(uploadCobol.flatMap { it.uploaded })
}

pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
    extensions.getByType(SourceSetContainer::class).named("main") {
        val convert = createConvertTask(uploadCobol)

        val kotlin = extensions.getByName<SourceDirectorySet>("kotlin")
        kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
    }
}

pluginManager.withPlugin("org.gradle.java") {
    extensions.getByType(SourceSetContainer::class).named("main") {
        val convert = createConvertTask(uploadCobol)
        java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
    }
}

fun SourceSet.createConvertTask(uploadTask: TaskProvider<out UploadTask>): TaskProvider<out KobolTask> {
    val taskName = "convertCobol"
    if (taskName in tasks.names) {
        return tasks.named(taskName, KobolTask::class)
    }

    val convert = tasks.register(taskName, KobolTask::class)

    val cobolSrc = objects.sourceDirectorySet("cobol", "cobol")
    cobolSrc.filter.include("*.cbl")
    cobolSrc.srcDir(file("src/${name}/cobol"))
    cobolSrc.destinationDirectory.convention(layout.buildDirectory.dir("generated/kobol/"))

    convert {
        classpath.from(kobolClasspath)
        sources.from(cobolSrc.sourceDirectories)
        outputFolder.convention(cobolSrc.destinationDirectory)
    }

    cobolSrc.compiledBy(convert, KobolTask::outputFolder)
    extensions.add("cobol", cobolSrc)

    uploadTask {
        files.from(cobolSrc)
    }

    return convert
}
