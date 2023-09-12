plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.fir2ir)

    testImplementation(kotlin("test"))
    testImplementation(projects.plugins.pluginInlining)
    testImplementation(projects.plugins.pluginNosynthetic)
}
