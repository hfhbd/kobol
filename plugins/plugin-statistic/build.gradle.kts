plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.5.0")
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(kotlin("test"))
}

serviceLoaders.register("app.softwork.kobol.fir.FirCodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.plugins.Factory")
}
