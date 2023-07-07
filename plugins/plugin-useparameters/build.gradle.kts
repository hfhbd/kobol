plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.ir)

    testImplementation(kotlin("test"))
}
