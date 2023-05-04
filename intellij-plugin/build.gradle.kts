plugins {
    id("setup")
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
    val idea = "221.6008.13"
    version.set("IU-$idea")
}

tasks {
    patchPluginXml {
        sinceBuild.set("221")
        untilBuild.set("231.*")
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
    val createPluginRepo by registering(CreatePluginRepo::class) {
        dependsOn(copyRepoPlugin, patchPluginXml)
        fileName.set(project.name)
        sinceBuild.set(patchPluginXml.flatMap { it.sinceBuild })
        untilBuild.set(patchPluginXml.flatMap { it.untilBuild })
    }
}
