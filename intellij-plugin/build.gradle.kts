plugins {
    id("kotlinSetup")
    id("repos")
    id("org.jetbrains.intellij")
}

dependencies {
    implementation(projects.lexerParser)
}

configurations.implementation {
    exclude("org.jetbrains.kotlin", "kotlin-stdlib")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-common")
    exclude("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
}

intellij {
    version.set(libs.versions.idea.map { "IC-$it" })
}

tasks {
    patchPluginXml {
        sinceBuild.set("231")
        untilBuild.set("232")
        version.set(project.version.toString())
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
