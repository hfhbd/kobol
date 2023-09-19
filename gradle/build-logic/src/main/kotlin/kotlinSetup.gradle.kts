plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader")
    id("com.google.devtools.ksp")
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

tasks.assemble {
    dependsOn(tasks.compileTestKotlin)
}
