plugins {
    `kotlin-dsl`
}

dependencies {
    val kotlin = "1.8.20"
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin")
    implementation("org.jetbrains.kotlin:kotlin-serialization:$kotlin")
    implementation("app.cash.licensee:licensee-gradle-plugin:1.7.0-SNAPSHOT")
    implementation("org.jetbrains.intellij.plugins:gradle-intellij-plugin:1.13.0")
    implementation("org.jetbrains.intellij.plugins:gradle-grammarkit-plugin:2022.3.1")
    implementation("com.github.johnrengelman:shadow:8.1.1")
    implementation("app.softwork.serviceloader:gradle-plugin:0.0.8")
    implementation("com.google.devtools.ksp:com.google.devtools.ksp.gradle.plugin:$kotlin-1.0.9")
}

kotlin.jvmToolchain(17)
