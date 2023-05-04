plugins {
    id("setup")
}

dependencies {
    api(projects.ir)
    api(projects.fir)

    api(libs.sqldelight.writer)

    testImplementation(kotlin("test"))
}
