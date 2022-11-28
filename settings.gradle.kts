pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
// enableFeaturePreview("STABLE_CONFIGURATION_CACHE")

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
