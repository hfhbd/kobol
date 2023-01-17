plugins {
    setup
    repos
}

dependencies {
    implementation(projects.kobolIr)

    testImplementation(kotlin("test"))
}
