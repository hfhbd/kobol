plugins {
    id("app.cash.licensee")
    id("publish")
}

publishing {
    when (name) {
        "gradle-plugin" -> Unit
        "intellij-plugin" -> publications.register<MavenPublication>("mavenJava") {
            from(components["intellijPlatform"])
            artifact(tasks.named("sourcesJar"))
            artifact(tasks.named("javadocJar"))
        }

        else -> publications.register<MavenPublication>("mavenJava") {
            from(components["java"])
        }
    }
}

licensee {
    allow("Apache-2.0")
}
