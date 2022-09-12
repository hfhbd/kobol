plugins {
    kotlin("jvm") version "1.7.20-RC" apply false
    kotlin("plugin.serialization") version "1.7.20-RC" apply false
    id("org.jetbrains.intellij") version "1.9.0" apply false
    id("org.jetbrains.grammarkit") version "2021.2.2" apply false
    `maven-publish`
    id("app.cash.licensee") version "1.5.0" apply false
    id("com.github.johnrengelman.shadow") version "7.1.2" apply false
}

group = "app.softwork"

allprojects {
    repositories {
        mavenCentral()
        maven(url = "https://www.jetbrains.com/intellij-repository/releases")
        maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
        maven(url = "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/kotlin-ide-plugin-dependencies/")
        maven(url = "https://maven.pkg.jetbrains.space/public/p/ktor/eap")
    }
}

subprojects {
    plugins.apply("maven-publish")
    plugins.apply("org.jetbrains.kotlin.jvm")
    plugins.apply("org.jetbrains.kotlin.plugin.serialization")
    plugins.apply("app.cash.licensee")

    the<app.cash.licensee.LicenseeExtension>().apply {
        allow("Apache-2.0")
    }

    configurations.all {
        exclude(group = "com.jetbrains.rd")
        exclude(group = "com.github.jetbrains", module = "jetCheck")
        exclude(group = "com.jetbrains.infra")
        exclude(group = "org.roaringbitmap")
        exclude(group = "ai.grazie.spell")
        exclude(group = "ai.grazie.utils")
        exclude(group = "ai.grazie.nlp")
    }
    tasks {
        // Set the JVM compatibility versions
        withType<JavaCompile> {
            sourceCompatibility = "11"
            targetCompatibility = "11"
        }
        withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
            kotlinOptions {
                jvmTarget = "11"
            }
        }
    }

    publishing {
        repositories {
            maven(url = "https://maven.pkg.github.com/hfhbd/kobol") {
                name = "GitHubPackages"
                credentials {
                    username = System.getenv("GITHUB_ACTOR")
                    password = System.getenv("GITHUB_TOKEN")
                }
            }
        }
        if (name != "kobol-gradle-plugin") {
            publications.register<MavenPublication>("mavenJava") {
                from(components["java"])
            }
        }
        publications.all {
            this as MavenPublication
            pom {
                name by "app.softwork KOBOL"
                url by "https://github.com/hfhbd/kobol"
                developers {
                    developer {
                        id by "hfhbd"
                        name by "Philip Wedemann"
                        email by "mybztg+mavencentral@icloud.com"
                    }
                }
                scm {
                    connection by "scm:git://github.com/hfhbd/kobol.git"
                    developerConnection by "scm:git://github.com/hfhbd/kobol.git"
                    url by "https://github.com/hfhbd/kobol"
                }
            }
        }
    }
}

infix fun <T> Property<T>.by(value: T) {
    set(value)
}
