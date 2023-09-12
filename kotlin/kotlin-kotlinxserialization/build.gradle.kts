plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.fir2ir)

    testImplementation(kotlin("test"))
}
