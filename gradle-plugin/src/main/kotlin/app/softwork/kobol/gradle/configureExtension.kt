package app.softwork.kobol.gradle

import org.gradle.api.Project
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.tasks.SourceSet
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.api.tasks.TaskProvider
import org.gradle.kotlin.dsl.*

internal fun Project.configureExtension(kobol: Kobol, sourceSetContainer: SourceSetContainer) {
    val kobolCompiler = configurations.dependencyScope("kobolCompiler") {
        it.fromDependencyCollector(kobol.dependencies.compiler)
    }

    kobol.dependencies {
        it.compiler.add("$GROUP:intellij-env:$VERSION")
    }

    val kobolCompilerClasspath = configurations.resolvable("kobolCompilerClasspath") {
        it.extendsFrom(kobolCompiler.get())
    }

    kobol.firActions.all { firAction ->
        val deps = configurations.dependencyScope("kobolFir${firAction.name}") {
            it.fromDependencyCollector(firAction.dependencies.compiler)
        }
        tasks.register("kobol${firAction.name}", KobolFirPluginTask::class.java) {
            it.pluginClasspath.from(
                configurations.resolvable("kobolFir${firAction.name}Classpath") {
                    it.extendsFrom(deps.get())
                },
            )
        }
    }

    val existingSourceSet = extensions.findByName("sourceSets")
    val sourceSets = if (existingSourceSet != null) {
        existingSourceSet as SourceSetContainer
    } else {
        extensions.add(SourceSetContainer::class.java, "sourceSets", sourceSetContainer)
        sourceSetContainer.register(SourceSet.MAIN_SOURCE_SET_NAME)
        sourceSetContainer.register(SourceSet.TEST_SOURCE_SET_NAME)
        sourceSetContainer
    }

    fun SourceSet.createCompilerTask(): TaskProvider<out KobolTask> {
        val sourceSetName = name.replaceFirstChar { it.uppercaseChar() }
        val taskName = "compileCobol$sourceSetName"
        if (taskName in tasks.names) {
            return tasks.named(taskName, KobolTask::class.java)
        }

        val cobolSrc = objects.sourceDirectorySet("cobol", "cobol")
        cobolSrc.filter.include("*.cbl")
        cobolSrc.srcDir(file("src/$name/cobol"))
        cobolSrc.destinationDirectory.convention(layout.buildDirectory.dir("generated/kobol/$name"))

        val convert = tasks.register(taskName, KobolTask::class.java) {
            it.classpath.from(kobolCompilerClasspath)
            it.sources.from(cobolSrc.sourceDirectories)
            it.outputFolder.convention(cobolSrc.destinationDirectory)
        }

        cobolSrc.compiledBy(convert, KobolTask::outputFolder)
        extensions.add("cobol", cobolSrc)

        return convert
    }

    pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
        sourceSets.configureEach {
            val convert = it.createCompilerTask()

            val kotlin = it.extensions.getByName("kotlin") as SourceDirectorySet
            kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
        }
    }

    pluginManager.withPlugin("org.gradle.java") {
        sourceSets.configureEach {
            val convert = it.createCompilerTask()
            it.java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
        }
    }
}
