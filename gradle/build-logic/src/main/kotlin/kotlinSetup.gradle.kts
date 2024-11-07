plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader-compiler")
}

kotlin {
    jvmToolchain(21)
    explicitApi()

    compilerOptions {
        freeCompilerArgs.add("-Xcontext-receivers")
        progressiveMode.set(true)
        // allWarningsAsErrors.set(true)
    }
}

publishing.publications.register<MavenPublication>("mavenJava") {
    from(components["java"])
}
