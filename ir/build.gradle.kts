plugins {
    id("kotlinSetup")
}

dependencies {
    api(libs.serialization.core)
    api(projects.util)

    testImplementation(kotlin("test"))
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.plugins.pluginExitprocess)
}
