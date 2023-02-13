plugins {
    setup
    repos
    org.jetbrains.intellij
}

dependencies {
    implementation(projects.lexerParser)
}

val idea = "221.6008.13"
// Configure Gradle IntelliJ Plugin - read more: https://github.com/JetBrains/gradle-intellij-plugin
intellij {
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
