pluginManagement {
    includeBuild("build-logic")
    repositories {
        maven(url = "https://oss.sonatype.org/content/repositories/snapshots")
        mavenCentral()
        gradlePluginPortal()
    }
}

plugins {
    id("MyRepos")
}

rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
enableFeaturePreview("STABLE_CONFIGURATION_CACHE")

include(":kobol-lexer-parser")
include(":kobol-fir")
include(":kobol-ir")

include(":kobol-kotlin")
include(":kobol-kotlin-sqldelight")
include(":kobol-kotlin-kotlinxserialization")
include(":kobol-kotlin-file-java")

include(":kobol-java")
include(":kobol-java-java8")
include(":kobol-java-jdbc")

include(":kobol-gradle-plugin")
include(":kobol-intellij-plugin")

include(":kobol-flow-graph")

include(":kobol-plugins:kobol-plugins-booleanExpressions")
include(":kobol-plugins:kobol-plugins-camelCase")
// include(":kobol-plugins:kobol-plugins-classes")
include(":kobol-plugins:kobol-plugins-constVariables")
include(":kobol-plugins:kobol-plugins-flow-graph-plantuml")
include(":kobol-plugins:kobol-plugins-ifAssignments")
include(":kobol-plugins:kobol-plugins-ktor")
include(":kobol-plugins:kobol-plugins-nullabletozero")
include(":kobol-plugins:kobol-plugins-objects")
include(":kobol-plugins:kobol-plugins-optimize")
include(":kobol-plugins:kobol-plugins-private")
include(":kobol-plugins:kobol-plugins-readOnlyVariables")
include(":kobol-plugins:kobol-plugins-useParameters")
