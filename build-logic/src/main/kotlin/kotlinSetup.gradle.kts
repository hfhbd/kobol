import app.softwork.github.dependencies.upload.*
import org.jetbrains.kotlin.gradle.dsl.KotlinJvmProjectExtension

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

pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
    extensions.configure<KotlinJvmProjectExtension>("kotlin") {
        jvmToolchain(11)
        explicitApi()
        target {
            compilations.configureEach {
                kotlinSourceSets.forAll {
                    it.languageSettings.progressiveMode = true
                }
                kotlinOptions {
                    // allWarningsAsErrors = true
                }
            }
        }
    }
}

licensee {
    allow("Apache-2.0")
}

tasks.register<GitHubDependenciesUpload>("uploadRuntimeToGitHub") {
    scope.set(Scope.Runtime)
    uploadConfiguration(configurations.runtimeClasspath)
}

tasks.register<GitHubDependenciesUpload>("uploadCompileToGitHub") {
    scope.set(Scope.Development)
    uploadConfiguration(configurations.compileClasspath)
}
