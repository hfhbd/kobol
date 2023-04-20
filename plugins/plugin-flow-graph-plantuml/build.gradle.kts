plugins {
    setup
}

dependencies {
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(kotlin("test"))
}
