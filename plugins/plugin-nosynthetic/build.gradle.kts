plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.plugins.pluginInliningUtil)

    testImplementation(kotlin("test"))
}
