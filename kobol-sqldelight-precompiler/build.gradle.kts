plugins {
    setup
    repos
}

dependencies {
    api(projects.kobolIr)
    api(projects.kobolFir)

    api("app.softwork:sqldelight-writer:0.0.2")

    testImplementation(kotlin("test"))
}
