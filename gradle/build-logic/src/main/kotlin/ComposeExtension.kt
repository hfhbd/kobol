import org.gradle.api.artifacts.ExternalDependency
import org.gradle.api.provider.Property

interface ComposeExtension {
    val kotlinCompilerPlugin: Property<ExternalDependency>
    val runtime: Property<ExternalDependency>
}
