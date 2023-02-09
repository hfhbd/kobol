plugins {
    setup
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")
    api(projects.fir)

    testImplementation(kotlin("test"))
}
