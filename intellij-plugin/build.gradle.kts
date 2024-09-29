import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType
import org.jetbrains.intellij.platform.gradle.TestFrameworkType

plugins {
    id("org.jetbrains.intellij.platform")
    id("kotlinSetup")
}

kotlin.jvmToolchain {
    vendor.set(JvmVendorSpec.JETBRAINS)
}

dependencies {
    implementation(projects.psi)

    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    compileOnly(libs.bundles.idea)

    intellijPlatform {
        intellijIdeaCommunity(libs.versions.idea, useInstaller = false)

        pluginVerifier("1.367")
        javaCompiler(libs.versions.idea)
        testFramework(TestFrameworkType.Platform, libs.versions.idea)
    }
}

configurations.runtimeClasspath {
    exclude("org.jetbrains.kotlin", "kotlin-stdlib")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-common")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
    exclude("org.jetbrains.kotlinx", "kotlinx-coroutines")
}

intellijPlatform {
    buildSearchableOptions.set(false)

    pluginConfiguration {
        this.version.set(project.version.toString())
        ideaVersion {
            sinceBuild.set("233")
            untilBuild.set("242.*")
        }
    }
    pluginVerification {
        ides {
            ide(IntelliJPlatformType.IntellijIdeaCommunity, "2024.1.4")
            ide(IntelliJPlatformType.AndroidStudio, "2024.1.2.10")
        }
    }
}

tasks {
    val copyRepoPlugin by registering(Copy::class) {
        dependsOn(buildPlugin)

        from(buildPlugin)
        into("build/customRepo")
    }
    register("createPluginRepo", CreatePluginRepo::class) {
        dependsOn(copyRepoPlugin, patchPluginXml)
        fileName.set(project.name)
        sinceBuild.set(patchPluginXml.flatMap { it.sinceBuild })
        untilBuild.set(patchPluginXml.flatMap { it.untilBuild })
    }
}
