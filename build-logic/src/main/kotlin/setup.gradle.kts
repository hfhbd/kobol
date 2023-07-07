import app.softwork.github.dependencies.upload.*

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
