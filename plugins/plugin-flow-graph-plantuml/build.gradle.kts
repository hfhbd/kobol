plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.fir)

    testImplementation(libs.bundles.idea)
    testImplementation(projects.builder)
}
