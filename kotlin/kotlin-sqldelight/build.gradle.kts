plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.fir2ir)

    api(libs.sqldelight.writer)

    testImplementation(kotlin("test"))
}
