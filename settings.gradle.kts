pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}

rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

include(":kobol-lexer-parser")
include(":kobol-ir")
include(":kobol-kotlin")
include(":kobol-java")

include(":kobol-gradle-plugin")
include(":kobol-intellij-plugin")
