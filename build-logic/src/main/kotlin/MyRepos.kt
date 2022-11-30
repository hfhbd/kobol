import org.gradle.api.*
import org.gradle.api.artifacts.dsl.*
import org.gradle.api.artifacts.repositories.*
import org.gradle.api.initialization.*
import org.gradle.kotlin.dsl.*

class MyRepos : Plugin<Settings> {
    override fun apply(settings: Settings) {
        settings.dependencyResolutionManagement {
            repositories {
                kobol()
            }
        }
    }
}

fun RepositoryHandler.kobol() {
    mavenCentral()
    maven(url = "https://maven.pkg.github.com/hfhbd/kobol") {
        name = "GitHubPackages"
        credentials(PasswordCredentials::class)
    }
    maven(url = "https://www.jetbrains.com/intellij-repository/releases")
    maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    maven(url = "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/kotlin-ide-plugin-dependencies/")
    maven(url = "https://maven.pkg.jetbrains.space/public/p/ktor/eap")
}
