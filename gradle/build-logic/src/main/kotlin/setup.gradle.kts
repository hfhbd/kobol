plugins {
    id("app.cash.licensee")
    id("publish")
}

publishing {
    if (name != "gradle-plugin") {
        publications.register<MavenPublication>("mavenJava") {
            from(components["java"])
        }
    }
}

licensee {
    allow("Apache-2.0")
}
