plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    id("setup")
    id("app.softwork.serviceloader-compiler")
    id("jvm-test-suite")
}

kotlin {
    jvmToolchain(21)
    explicitApi()

    compilerOptions {
        freeCompilerArgs.add("-Xcontext-parameters")
        progressiveMode.set(true)
    }
}

publishing.publications.register<MavenPublication>("mavenJava") {
    from(components["java"])
}

testing.suites.named("test", JvmTestSuite::class) {
    useKotlinTest()
}
