package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.plugins.JavaPlugin.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(project: Project) {
        val kobolIrPlugin = project.configurations.register("kobolIrPlugin") {
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
        }

        val kobolFirPlugin = project.configurations.register("kobolFirPlugin") {
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
        }

        val kobolFlowGraphPlugin = project.configurations.register("kobolFlowGraphPlugin") {
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
            
            it.defaultDependencies {
                it.add(project.dependencies.create("app.softwork:kobol-plugins-flow-graph-plantuml:$kobolVersion"))
            }
        }

        val convert = project.tasks.register("convertCobolToKotlin", KobolTask::class.java) {
            it.classpath.from(kobolIrPlugin, kobolFirPlugin)
        }

        val convertJava = project.tasks.register("convertCobolToJava", JavaKobolTask::class.java) {
            it.classpath.from(kobolIrPlugin, kobolFirPlugin)
        }

        project.plugins.withId("org.jetbrains.kotlin.jvm") {
            project.tasks.named("compileKotlin") { it.dependsOn(convert) }
        }

        project.plugins.withId("org.gradle.java") {
            project.tasks.named(COMPILE_JAVA_TASK_NAME) { it.dependsOn(convertJava) }
        }

        val upload = project.tasks.register("uploadCobol", UploadTask::class.java)
        project.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
        }

        project.tasks.register("flowGraph", KobolFlowGraph::class.java) {
            it.classpath.from(kobolFirPlugin, kobolFlowGraphPlugin)
        }
    }
}
