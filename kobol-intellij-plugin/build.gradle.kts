plugins {
    id("org.jetbrains.intellij")
}

dependencies {
    implementation(projects.kobolLexerParser)
}

// Configure Gradle IntelliJ Plugin - read more: https://github.com/JetBrains/gradle-intellij-plugin
intellij {
    version.set("2022.2")
    type.set("IU") // Target IDE Platform
}

tasks {
    patchPluginXml {
        sinceBuild.set("222")
        untilBuild.set("223.*")
        version.set(project.version.toString())
    }

    val copyRepoPlugin by registering(Copy::class) {
        dependsOn(buildPlugin)
        copy {
            from("build/distributions/kobol-intellij-plugin-$version.zip")
            into("build/customRepo")
        }
    }

    val createPluginRepo by registering {
        dependsOn(copyRepoPlugin, patchPluginXml)
        doFirst {
            val xml = File(File(buildDir, "customRepo"), "updatePlugins.xml")
            xml.writeText(
                """
                <plugins>
                <plugin id="app.softwork.kobol" url="https://hfhbd.github.io/kobol/kobol-intellij-plugin-$version.zip" version="$version">
                <idea-version since-build="${patchPluginXml.get().sinceBuild.get()}" until-build="${patchPluginXml.get().untilBuild.get()}"/>
                </plugin>
                </plugins>
            """.trimIndent()
            )
        }
    }
}
