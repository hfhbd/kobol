import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType
import org.jetbrains.intellij.platform.gradle.TestFrameworkType

plugins {
    id("org.jetbrains.intellij.platform")
    kotlin("jvm")
    id("setup")
}

kotlin.jvmToolchain {
    languageVersion.set(JavaLanguageVersion.of(21))
    vendor.set(JvmVendorSpec.JETBRAINS)
}

publishing.publications.register<MavenPublication>("mavenJava") {
    from(components["intellijPlatform"])
    artifact(tasks.named("sourcesJar"))
    artifact(tasks.named("javadocJar"))
}

dependencies {
    implementation(projects.psi)

    intellijPlatform {
        intellijIdeaCommunity(libs.versions.ideaplugin, useInstaller = false)

        pluginVerifier()
        javaCompiler(libs.versions.ideaplugin)
        testFramework(TestFrameworkType.Platform, libs.versions.ideaplugin)
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
            sinceBuild.set("242")
            untilBuild.set("243.*")
        }
    }
    pluginVerification {
        ides {
            ide(IntelliJPlatformType.IntellijIdeaCommunity, "2024.2.4")
            ide(IntelliJPlatformType.AndroidStudio, "2024.2.2.9")
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
