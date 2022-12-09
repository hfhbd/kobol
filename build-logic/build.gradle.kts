plugins {
    `kotlin-dsl`
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:1.8.0-Beta")
    implementation("org.jetbrains.kotlin:kotlin-serialization:1.8.0-RC")
    implementation("app.cash.licensee:licensee-gradle-plugin:1.6.0")
    implementation("org.jetbrains.intellij.plugins:gradle-intellij-plugin:1.10.0")
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
