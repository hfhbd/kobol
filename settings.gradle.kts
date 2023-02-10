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
    id("com.gradle.enterprise") version "3.12.3"
}

gradleEnterprise {
    buildScan {
        termsOfServiceUrl = "https://gradle.com/terms-of-service"
        termsOfServiceAgree = "yes"
        if (System.getenv("CI") != null) {
            publishAlways()
            tag("CI")
        }
    }
}

rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
enableFeaturePreview("STABLE_CONFIGURATION_CACHE")

include(":lexer-parser")
include(":fir")
include(":ir")

include(":kotlin")
include(":kotlin:kotlin-sqldelight")
include(":kotlin:kotlin-kotlinxserialization")
include(":kotlin:kotlin-file-java")

include(":java")
include(":java:java-java8")
include(":java:java-jdbc")

include(":gradle-plugin")
include(":intellij-plugin")

include(":flow-graph")

include(":plugins:plugin-booleanexpressions")
include(":plugins:plugin-javanames")
include(":plugins:plugin-keepnames")
// include("plugin-classes")
include(":plugins:plugin-constvariables")
include(":plugins:plugin-flow-graph-plantuml")
include(":plugins:plugin-ifassignments")
include(":plugins:plugin-inlining")
include(":plugins:plugin-ktor")
include(":plugins:plugin-nullabletozero")
include(":plugins:plugin-objects")
include(":plugins:plugin-optimize")
include(":plugins:plugin-private")
include(":plugins:plugin-renaming")
include(":plugins:plugin-readonlyvariables")
include(":plugins:plugin-useparameters")
