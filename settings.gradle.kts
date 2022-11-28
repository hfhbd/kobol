pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
    includeBuild("build-logic")
}

plugins {
    id("MyRepos")
}

rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
// enableFeaturePreview("STABLE_CONFIGURATION_CACHE")

includeBuild("build-logic")

include(":kobol-lexer-parser")
include(":kobol-fir")
include(":kobol-ir")

include(":kobol-kotlin")
include(":kobol-sqldelight-precompiler")
include(":kobol-kotlinx-serialization")
include(":kobol-java-file-kotlin")

include(":kobol-java")
include(":kobol-jdbc")

include(":kobol-gradle-plugin")
include(":kobol-intellij-plugin")

include(":kobol-flow-graph")

include(":kobol-plugins:kobol-plugins-nullabletozero")
include(":kobol-plugins:kobol-plugins-flow-graph-plantuml")
