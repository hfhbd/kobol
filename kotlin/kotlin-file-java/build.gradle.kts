plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.ir)
    api(projects.fir)

    testImplementation(kotlin("test"))
}
