plugins {
    id("setup")
}

dependencies {
    api(projects.ir)
    api(projects.fir)

    api("app.softwork:sqldelight-writer:0.0.2")

    testImplementation(kotlin("test"))
}
