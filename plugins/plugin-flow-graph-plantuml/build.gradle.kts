plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.fir)

    testImplementation(projects.builder)
    testImplementation(kotlin("test"))
}
