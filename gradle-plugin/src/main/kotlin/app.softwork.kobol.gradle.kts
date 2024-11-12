import app.softwork.kobol.gradle.*

val kobol: Kobol = extensions.create<Kobol>("kobol")

val kobolCompiler = configurations.dependencyScope("kobolCompiler") {
    fromDependencyCollector(kobol.dependencies.compiler)
}

kobol.dependencies {
    compiler("$GROUP:intellij-env:$VERSION")
}

val kobolCompilerClasspath = configurations.resolvable("kobolCompilerClasspath") {
    extendsFrom(kobolCompiler.get())
}

kobol.firActions.all {
    val deps = configurations.dependencyScope("kobolFir$name") {
        fromDependencyCollector(this@all.dependencies.compiler)
    }
    this@all.dependencies {
        compiler("$GROUP:intellij-env:$VERSION")
    }
    tasks.register("kobol$name", KobolFirPluginTask::class) {
        pluginClasspath.from(
            pluginClasspath.from(
                configurations.resolvable("kobolFir${name}Classpath") {
                    extendsFrom(deps.get())
                },
            )
        )
    }
}

pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
    extensions.getByType(SourceSetContainer::class).configureEach {
        val convert = createCompilerTask()

        val kotlin = extensions.getByName<SourceDirectorySet>("kotlin")
        kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
    }
}

pluginManager.withPlugin("org.gradle.java") {
    extensions.getByType(SourceSetContainer::class).configureEach {
        val convert = createCompilerTask()
        java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
    }
}

fun SourceSet.createCompilerTask(): TaskProvider<out KobolTask> {
    val sourceSetName = name.replaceFirstChar { it.uppercaseChar() }
    val taskName = "compileCobol$sourceSetName"
    if (taskName in tasks.names) {
        return tasks.named(taskName, KobolTask::class)
    }

    val cobolSrc = objects.sourceDirectorySet("cobol", "cobol")
    cobolSrc.filter.include("*.cbl")
    cobolSrc.srcDir(file("src/${name}/cobol"))
    cobolSrc.destinationDirectory.convention(layout.buildDirectory.dir("generated/kobol/"))

    val convert = tasks.register(taskName, KobolTask::class) {
        classpath.from(kobolCompilerClasspath)
        sources.from(cobolSrc.sourceDirectories)
        outputFolder.convention(cobolSrc.destinationDirectory)
    }

    cobolSrc.compiledBy(convert, KobolTask::outputFolder)
    extensions.add("cobol", cobolSrc)

    return convert
}
