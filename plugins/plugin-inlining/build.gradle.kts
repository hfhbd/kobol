plugins {
    setup
}

dependencies {
    api(projects.plugins.pluginInliningUtil)

    testImplementation(kotlin("test"))
}
