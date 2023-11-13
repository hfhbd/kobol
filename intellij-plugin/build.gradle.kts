plugins {
    kotlin("jvm") version "1.9.20"
    id("org.jetbrains.intellij") version "1.16.0"
}

intellij {
    version.set(libs.versions.idea.map { "IC-$it" })
}

repositories {
    mavenCentral()

    maven(url = "https://www.jetbrains.com/intellij-repository/releases")
    maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    maven(url = "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/kotlin-ide-plugin-dependencies/")

}