plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader")
    id("com.google.devtools.ksp")
    id("compose")
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

val libs = extensions.getByType<VersionCatalogsExtension>().named("libs")

extensions.getByName<ComposeExtension>("compose").apply {
    kotlinCompilerPlugin.set(libs.findLibrary("compose-compiler").get())
    runtime.set(libs.findLibrary("compose-runtime").get())
}
