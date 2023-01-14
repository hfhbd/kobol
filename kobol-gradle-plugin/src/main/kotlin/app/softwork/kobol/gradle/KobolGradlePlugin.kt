package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.Named
import org.gradle.api.plugins.JavaPlugin.*
import javax.inject.*

public class KobolGradlePlugin : Plugin<Project> {
    override fun apply(project: Project) {
        val kobol = project.extensions.create("kobol", Kobol::class.java)
        kobol.cobolSources.register("testing.cbl")
        kobol.cobolSources.register("main.cbl")
        kobol.cobolSources.register("hello.cbl")

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
        val buildCobol = project.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
        }
        project.tasks.register("runCobol", KobolRunTask::class.java) {
            it.dependsOn(buildCobol)
        }

        project.tasks.register("flowGraph", KobolFlowGraph::class.java) {
            it.classpath.from(kobolFirPlugin, kobolFlowGraphPlugin)
        }
        project.tasks.register("cleanCobol", CleanCobol::class.java)
    }
}

public interface Kobol {
    public val cobolSources: NamedDomainObjectContainer<CobolSource>
}

public abstract class CobolSource @Inject constructor(name: String, project: Project) : Named {
    private val name = name.toLowerCase().split(".").first()
    private val nameTitle = this.name.let {
        it[0].toUpperCase() + it.substring(1, it.length)
    }

    override fun getName(): String = name
    private val plugins = project.configurations.register("kobol${nameTitle}Plugin") {
        it.isCanBeResolved = true
        it.isCanBeConsumed = false

        it.defaultDependencies {
            it.add(project.dependencies.create("app.softwork:kobol-plugins-flow-graph-plantuml:$kobolVersion"))
        }
    }
} 
