import org.jetbrains.intellij.platform.gradle.extensions.intellijPlatform

plugins {
    id("org.jetbrains.intellij.platform.settings")
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        mavenCentral()
        intellijPlatform {
            defaultRepositories()
        }
    }
}
