plugins {
    setup
    repos
    app.softwork.serviceloader
}

dependencies {
    api(projects.kobolIr)

    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.ir.IrPlugin") {
    implementationClasses.add("app.softwork.kobol.plugins.ir.optimizations.Objects")
}
