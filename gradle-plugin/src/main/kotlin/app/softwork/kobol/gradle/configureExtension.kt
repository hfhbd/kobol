package app.softwork.kobol.gradle

import org.gradle.api.Project
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.tasks.SourceSet
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.api.tasks.TaskProvider
import org.gradle.kotlin.dsl.*

internal fun Project.configureExtension(kobol: Kobol, sourceSetContainer: SourceSetContainer) {
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

    val existingSourceSet = extensions.findByName("sourceSets")
    val sourceSets = if (existingSourceSet != null) {
        existingSourceSet as SourceSetContainer
    } else {
        extensions.add(SourceSetContainer::class, "sourceSets", sourceSetContainer)
        sourceSetContainer.register(SourceSet.MAIN_SOURCE_SET_NAME)
        sourceSetContainer.register(SourceSet.TEST_SOURCE_SET_NAME)
        sourceSetContainer
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

    pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
        sourceSets.configureEach {
            val convert = createCompilerTask()

            val kotlin = extensions.getByName<SourceDirectorySet>("kotlin")
            kotlin.srcDir(convert.flatMap { it.outputFolder.dir("kotlin") })
        }
    }

    pluginManager.withPlugin("org.gradle.java") {
        sourceSets.configureEach {
            val convert = createCompilerTask()
            java.srcDir(convert.flatMap { it.outputFolder.dir("java") })
        }
    }
}
