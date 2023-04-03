plugins {
    setup
    repos
    org.jetbrains.intellij
}

dependencies {
    implementation(projects.lexerParser) {
        exclude("org.jetbrains.kotlin", "kotlin-stdlib")
        exclude("org.jetbrains.kotlin", "kotlin-stdlib-common")
        exclude("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
    }
}

intellij {
    version.set("IU-231.8109.175")
}

tasks {
    patchPluginXml {
        sinceBuild.set("231")
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
