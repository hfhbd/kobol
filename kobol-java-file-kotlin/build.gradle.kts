plugins {
    setup
}

dependencies {
    api(projects.kobolIr)
    api(projects.kobolFir)

    testImplementation(kotlin("test"))
}
