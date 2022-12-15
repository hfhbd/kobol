plugins {
    setup
    repos
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")
    api(projects.kobolFir)

    testImplementation(kotlin("test"))
}
