plugins {
    `kotlin-dsl`
}

dependencies {
    val kotlin = "1.8.10"
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin")
    implementation("org.jetbrains.kotlin:kotlin-serialization:$kotlin")
    implementation("app.cash.licensee:licensee-gradle-plugin:1.7.0-SNAPSHOT")
    implementation("org.jetbrains.intellij.plugins:gradle-intellij-plugin:1.13.0")
    implementation("org.jetbrains.intellij.plugins:gradle-grammarkit-plugin:2022.3.1")
    implementation("gradle.plugin.com.github.johnrengelman:shadow:8.0.0")
    implementation("app.softwork:serviceloader-gradle-plugin:0.0.2")
}

kotlin.jvmToolchain(17)
