plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.builder)

    testImplementation(projects.plugins.pluginInlining)
    testImplementation(projects.plugins.pluginNosynthetic)
}
