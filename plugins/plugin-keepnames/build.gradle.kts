plugins {
    setup
}

dependencies {
    api(projects.plugins.pluginRenaming)

    testImplementation(kotlin("test"))
}
