plugins {
    `kotlin-dsl`
}

dependencies {
    val kotlin = "1.8.0"
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin")
    implementation("org.jetbrains.kotlin:kotlin-serialization:$kotlin")
    implementation("app.cash.licensee:licensee-gradle-plugin:1.7.0-SNAPSHOT")
    implementation("org.jetbrains.intellij:org.jetbrains.intellij.gradle.plugin:1.12.1-SNAPSHOT")
    implementation("org.jetbrains.intellij.plugins:gradle-grammarkit-plugin:2022.3")
    implementation("gradle.plugin.com.github.johnrengelman:shadow:7.1.2")
    implementation("app.softwork:serviceloader-gradle-plugin:0.0.1")
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

gradlePlugin {
    plugins {
        register("MyRepos") {
            id = "MyRepos"
            implementationClass = "MyRepos"
        }
    }
}
