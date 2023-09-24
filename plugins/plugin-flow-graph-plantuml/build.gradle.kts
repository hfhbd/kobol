plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.builder)
    testImplementation(kotlin("test"))
}
