import org.gradle.api.artifacts.dsl.*
import org.gradle.api.artifacts.repositories.*
import org.gradle.kotlin.dsl.*

fun RepositoryHandler.kobol() {
    mavenCentral()
    maven(url = "https://maven.pkg.github.com/hfhbd/kobol") {
        name = "GitHubPackages"
        credentials(PasswordCredentials::class)
    }
    maven(url = "https://oss.sonatype.org/content/repositories/snapshots")
    maven(url = "https://s01.oss.sonatype.org/content/repositories/snapshots")

    maven(url = "https://www.jetbrains.com/intellij-repository/releases")
    maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    maven(url = "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/kotlin-ide-plugin-dependencies/")
}
