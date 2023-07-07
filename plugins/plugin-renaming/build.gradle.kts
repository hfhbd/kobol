plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.fir)

    testImplementation(kotlin("test"))
}
