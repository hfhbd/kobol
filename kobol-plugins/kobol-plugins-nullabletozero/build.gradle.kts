plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.kobolFir)

    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.fir.FirPluginBeforePhase") {
    implementationClasses.add("app.softwork.kobol.plugins.fir.NullableToZero")
}
