plugins {
    setup
}

dependencies {
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(kotlin("test"))
}
