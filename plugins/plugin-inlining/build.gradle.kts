plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.plugins.pluginInliningUtil)

    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.ir.IrPlugin") {
    implementationClasses.add("app.softwork.kobol.plugins.ir.optimizations.Inlining")
}
