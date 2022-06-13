plugins {
    id("org.jetbrains.intellij")
}

dependencies {
    implementation(projects.kobolLexerParser)
}

// Configure Gradle IntelliJ Plugin - read more: https://github.com/JetBrains/gradle-intellij-plugin
intellij {
    version.set("2022.1")
    type.set("IU") // Target IDE Platform
}

tasks {
    patchPluginXml {
        sinceBuild.set("221")
        untilBuild.set("222.*")
        version.set(project.version.toString())
    }
}
