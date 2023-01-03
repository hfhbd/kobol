dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        maven(url = "https://oss.sonatype.org/content/repositories/snapshots")
        mavenCentral()
        gradlePluginPortal()
    }
}

rootProject.name = "build-logic"

println("ACTIONS_CACHE_URL: ${System.getenv("ACTIONS_CACHE_URL")}")
