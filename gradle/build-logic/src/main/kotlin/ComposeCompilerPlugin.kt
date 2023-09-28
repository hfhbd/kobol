import org.gradle.api.Project
import org.gradle.api.artifacts.ExternalModuleDependency
import org.gradle.api.plugins.JavaPlugin.API_CONFIGURATION_NAME
import org.gradle.api.provider.Provider
import org.gradle.kotlin.dsl.getByName
import org.jetbrains.kotlin.gradle.plugin.KotlinCompilation
import org.jetbrains.kotlin.gradle.plugin.KotlinCompilerPluginSupportPlugin
import org.jetbrains.kotlin.gradle.plugin.SubpluginArtifact
import org.jetbrains.kotlin.gradle.plugin.SubpluginOption

class ComposeCompilerPlugin : KotlinCompilerPluginSupportPlugin {
    private lateinit var extension: ComposeExtension
    override fun apply(target: Project) {
        target.pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
            extension = target.extensions.getByName<ComposeExtension>("compose")
            target.dependencies.addProvider(API_CONFIGURATION_NAME, extension.runtime)
        }
    }

    override fun isApplicable(kotlinCompilation: KotlinCompilation<*>): Boolean = true

    override fun getCompilerPluginId(): String = "app.softwork.compose"

    override fun getPluginArtifact(): SubpluginArtifact {
        val kotlinCompilerPlugin = extension.kotlinCompilerPlugin.get()
        return SubpluginArtifact(
            kotlinCompilerPlugin.group,
            kotlinCompilerPlugin.name,
            kotlinCompilerPlugin.version
        )
    }

    override fun applyToCompilation(kotlinCompilation: KotlinCompilation<*>): Provider<List<SubpluginOption>> {
        return kotlinCompilation.target.project.provider { emptyList() }
    }
}
