plugins {
    id("kotlinSetup")
    id("org.jetbrains.intellij.platform")
}

dependencies {
    implementation(projects.psi)

    intellijPlatform {
        intellijIdeaCommunity("2023.3")

        pluginVerifier()
        instrumentationTools()
    }
}

configurations.implementation {
    exclude("org.jetbrains.kotlin", "kotlin-stdlib")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-common")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
}

tasks {
    patchPluginXml {
        sinceBuild.set("231")
        untilBuild.set("233.*")
        pluginVersion.set(project.version.toString())
    }

    buildSearchableOptions {
        enabled = false
    }

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
