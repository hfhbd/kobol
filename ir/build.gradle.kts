plugins {
    setup
}

dependencies {
    api(projects.fir)

    testImplementation(kotlin("test"))
    testImplementation(projects.plugins.pluginExitprocess)
}
