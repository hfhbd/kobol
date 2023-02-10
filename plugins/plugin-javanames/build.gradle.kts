plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.plugins.pluginRenaming)

    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.fir.FirPluginBeforePhase") {
    implementationClasses.add("app.softwork.kobol.plugins.fir.renaming.JavaNames")
}
