plugins {
    setup
    intellijTesting
    app.softwork.serviceloader
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.4.1")
    implementation(projects.fir)
}

serviceLoaders.register("app.softwork.kobol.fir.FirCodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.plugins.Factory")
}
