pluginManagement {
    includeBuild("gradle/build-logic")
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

plugins {
    id("MyRepos")
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.7.0"
    id("com.gradle.enterprise") version "3.14.1"
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
include(":catalog")

include(":plugins:plugin-booleanexpressions")
include(":plugins:plugin-javanames")
include(":plugins:plugin-keepnames")
include(":plugins:plugin-constvariables")
include(":plugins:plugin-flow-graph-plantuml")
include(":plugins:plugin-ifassignments")
include(":plugins:plugin-inlining")
include(":plugins:plugin-inlining-util")
include(":plugins:plugin-main-util")
include(":plugins:plugin-nosynthetic")
include(":plugins:plugin-nullabletozero")
include(":plugins:plugin-objects")
include(":plugins:plugin-optimize")
include(":plugins:plugin-private")
include(":plugins:plugin-renaming")
include(":plugins:plugin-readonlyvariables")
include(":plugins:plugin-statistic")
include(":plugins:plugin-useparameters")
include(":plugins:plugin-exitprocess")

include(":intellij-env")
include(":ssh-env")
