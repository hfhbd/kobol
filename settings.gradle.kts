pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

rootProject.name = "kobol"

enableFeaturePreview("STABLE_CONFIGURATION_CACHE")

include(":intellij-plugin")
