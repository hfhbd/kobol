plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.fir.FirCodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.flowgraph.Factory")
}
