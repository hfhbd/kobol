package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.plugins.JavaPlugin.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(target: Project) {
        val kobolIrPlugin = target.configurations.create("kobolIrPlugin") {
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
        }

        val kobolFirPlugin = target.configurations.create("kobolFirPlugin") {
            it.isCanBeResolved = true
            it.isCanBeConsumed = false
        }

        val convert = target.tasks.register("convertCobolToKotlin", KobolTask::class.java) {
            it.classpath.from(kobolIrPlugin, kobolFirPlugin)
        }

        val convertJava = target.tasks.register("convertCobolToJava", JavaKobolTask::class.java) {
            it.classpath.from(kobolIrPlugin, kobolFirPlugin)
        }

        target.plugins.withId("org.jetbrains.kotlin.jvm") {
            target.tasks.getByName("compileKotlin").dependsOn(convert)
        }

        target.plugins.withId("org.gradle.java") {
            target.tasks.getByName(COMPILE_JAVA_TASK_NAME).dependsOn(convertJava)
        }

        val upload = target.tasks.register("uploadCobol", UploadTask::class.java)
        target.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
        }

        target.tasks.register("flowGraph", KobolFlowGraph::class.java) {
            it.classpath.from(kobolFirPlugin.defaultDependencies {
                it.add(target.dependencies.create("app.softwork:kobol-flow-graph:$version"))
            })
        }
    }
}
