plugins {
    id("setup")
}

dependencies {
    api(projects.plugins.pluginRenaming)

    testImplementation(kotlin("test"))
}
