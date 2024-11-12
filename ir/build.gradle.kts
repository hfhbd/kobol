plugins {
    id("kotlinSetup")
}

dependencies {
    api(libs.serialization.core)
    api(projects.util)

    testImplementation(libs.bundles.idea)
    testImplementation(projects.plugins.pluginExitprocess)
}
