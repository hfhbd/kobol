plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader-compiler")
}

kotlin {
    jvmToolchain(17)
    explicitApi()

    compilerOptions {
        freeCompilerArgs.add("-Xcontext-receivers")
        progressiveMode.set(true)
        // allWarningsAsErrors.set(true)
    }
}
