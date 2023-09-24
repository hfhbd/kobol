plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.builder)

    api(libs.sqldelight.writer)

    testImplementation(kotlin("test"))
}
