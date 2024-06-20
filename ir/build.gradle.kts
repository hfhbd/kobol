plugins {
    id("kotlinSetup")
    id("java-test-fixtures")
}

dependencies {
    api(libs.serialization.core)
    api(projects.util)

    testFixturesApi(projects.builder)

    testImplementation(kotlin("test"))
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.plugins.pluginExitprocess)
}
