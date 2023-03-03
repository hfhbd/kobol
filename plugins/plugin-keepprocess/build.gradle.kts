plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.fir)

    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.fir.FirPlugin") {
    implementationClasses.add("app.softwork.kobol.plugins.fir.RemoveExitProcess")
}
