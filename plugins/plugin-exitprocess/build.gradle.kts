plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.ir)

    testImplementation(kotlin("test"))
    testImplementation(projects.plugins.pluginInlining)
    testImplementation(projects.plugins.pluginNosynthetic)
}

serviceLoaders.register("app.softwork.kobol.ir.ControlFlowHandlingFactory") {
    implementationClasses.add("app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory")
}
