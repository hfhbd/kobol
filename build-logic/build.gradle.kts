plugins {
    `kotlin-dsl`
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:1.8.0-RC")
    implementation("org.jetbrains.kotlin:kotlin-serialization:1.8.0-RC2")
    implementation("app.cash.licensee:licensee-gradle-plugin:1.7.0-SNAPSHOT")
    implementation("org.jetbrains.intellij.plugins:gradle-intellij-plugin:1.10.1")
    implementation("org.jetbrains.intellij.plugins:gradle-grammarkit-plugin:2022.3")
    implementation("gradle.plugin.com.github.johnrengelman:shadow:7.1.2")
}

gradlePlugin {
    plugins {
        create("MyRepos") {
            id = "MyRepos"
            implementationClass = "MyRepos"
        }
        create("repos") {
            id = "repos"
            implementationClass = "Repos"
        }
    }
}
