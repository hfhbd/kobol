dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        mavenCentral()
        gradlePluginPortal()
        google()
    }

    versionCatalogs.register("libs") {
        from(files("../libs.versions.toml"))
    }
}

rootProject.name = "build-logic"
