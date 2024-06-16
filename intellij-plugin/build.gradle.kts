import org.jetbrains.intellij.platform.gradle.TestFrameworkType

plugins {
    id("kotlinSetup")
    id("org.jetbrains.intellij.platform")
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
        intellijIdeaCommunity(libs.versions.idea)

        pluginVerifier("1.365")
        javaCompiler(libs.versions.idea)
        testFramework(TestFrameworkType.Platform, libs.versions.idea.get())
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
            untilBuild.set("241.*")
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
