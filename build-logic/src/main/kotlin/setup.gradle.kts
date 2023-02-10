plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("kotlinSetup")
    `maven-publish`
}

publishing {
    publications.register<MavenPublication>("mavenJava") {
        from(components["java"])
    }
}

kotlin {
    jvmToolchain(17)
    explicitApi()
    target {
        compilations.configureEach {
            kotlinSourceSets.forAll {
                it.languageSettings {
                    progressiveMode = true
                    languageVersion = "2.0"
                }
            }
            kotlinOptions {
                freeCompilerArgs += "-Xcontext-receivers"
                freeCompilerArgs += "-Xskip-prerelease-check"
                // allWarningsAsErrors = true
            }
        }
    }
}
