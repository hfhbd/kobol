plugins {
    id("setup")
}

dependencies {
    api(projects.fir)

    testImplementation(kotlin("test"))
}
