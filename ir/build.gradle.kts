plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.fir)

    testImplementation(kotlin("test"))
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.plugins.pluginExitprocess)
}
