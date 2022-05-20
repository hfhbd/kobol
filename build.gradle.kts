plugins {
    id("org.jetbrains.kotlin.jvm") version "1.6.21" apply false
    id("org.jetbrains.intellij") version "1.5.3" apply false
    id("org.jetbrains.grammarkit") version "2021.2.2" apply false
    id("io.gitlab.arturbosch.detekt") version "1.20.0"
}

group = "app.softwork"
version = "1.0-SNAPSHOT"

allprojects {
    repositories {
        mavenCentral()
        maven(url = "https://www.jetbrains.com/intellij-repository/releases")
        maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    }
}
