plugins {
    setup
}

dependencies {
    api(projects.ir)

    testImplementation(kotlin("test"))
    testImplementation(projects.plugins.pluginInlining)
    testImplementation(projects.plugins.pluginNosynthetic)
}
