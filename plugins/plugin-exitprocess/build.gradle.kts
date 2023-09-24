plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.builder)

    testImplementation(kotlin("test"))
    testImplementation(projects.plugins.pluginInlining)
    testImplementation(projects.plugins.pluginNosynthetic)
}
