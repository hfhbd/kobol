plugins {
    kotlin("jvm") version "1.6.21" apply false
    id("org.jetbrains.intellij") version "1.6.0" apply false
    id("org.jetbrains.grammarkit") version "2021.2.2" apply false
    `maven-publish`
}

allprojects {
    group = "app.softwork"
    version = "1.0-SNAPSHOT"

    repositories {
        mavenCentral()
        maven(url = "https://www.jetbrains.com/intellij-repository/releases")
        maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    }
    plugins.apply("org.jetbrains.kotlin.jvm")
    plugins.apply("org.gradle.maven-publish")

    val emptyJar by tasks.creating(Jar::class) { }

    group = "app.softwork"

    publishing {
        publications.create<MavenPublication>("mavenJava") {
            from(components["java"])
        }
        publications.all {
            this as MavenPublication
            artifact(emptyJar) {
                classifier = "javadoc"
            }
            pom {
                name.set("app.softwork UUID Library")
                description.set("A multiplatform Kotlin CSV and FLF serialization library")
                url.set("https://github.com/hfhbd/kotlinx-serialization-csv")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("hfhbd")
                        name.set("Philip Wedemann")
                        email.set("mybztg+mavencentral@icloud.com")
                    }
                }
                scm {
                    connection.set("scm:git://github.com/hfhbd/kotlinx-serialization-csv.git")
                    developerConnection.set("scm:git://github.com/hfhbd/kotlinx-serialization-csv.git")
                    url.set("https://github.com/hfhbd/kotlinx-serialization-csv")
                }
            }
        }
    }
}
