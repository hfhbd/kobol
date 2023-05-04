plugins {
    id("setup")
}

dependencies {
    implementation(libs.serialization.json)
    implementation(projects.fir)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(kotlin("test"))
}
