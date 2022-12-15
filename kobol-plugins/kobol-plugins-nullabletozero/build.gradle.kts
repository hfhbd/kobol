plugins {
    setup
    repos
}

dependencies {
    api(projects.kobolFir)

    testImplementation(kotlin("test"))
}
