plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(libs.serialization.json)
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(kotlin("test"))
    testImplementation(projects.builder)
}
