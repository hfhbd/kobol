plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(libs.serialization.json)
    implementation(projects.fir)

    testImplementation(libs.bundles.idea)
    testImplementation(projects.builder)
}
