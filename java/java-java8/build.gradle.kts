plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.ir)

    testImplementation(kotlin("test"))
}
