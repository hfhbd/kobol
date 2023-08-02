plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader")
    id("com.google.devtools.ksp")
}

kotlin {
    jvmToolchain(17)
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

tasks.assemble {
    dependsOn(tasks.compileTestKotlin)
}