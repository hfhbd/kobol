import org.jetbrains.kotlin.gradle.dsl.KotlinJvmProjectExtension

plugins {
    app.cash.licensee
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
}

licensee {
    allow("Apache-2.0")
}
