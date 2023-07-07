plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.plugins.pluginRenaming)

    testImplementation(kotlin("test"))
}
