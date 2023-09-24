plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.builder)

    testImplementation(kotlin("test"))
}
