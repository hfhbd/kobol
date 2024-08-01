plugins {
    id("app.cash.licensee")
    id("publish")
}

publishing {
    when (name) {
        "gradle-plugin" -> Unit
        "intellij-plugin" -> publications.register<MavenPublication>("mavenJava") {
            from(components["intellijPlatform"])
        }

        else -> publications.register<MavenPublication>("mavenJava") {
            from(components["java"])
        }
    }
}

licensee {
    allow("Apache-2.0")
}
