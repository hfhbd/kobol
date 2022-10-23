dependencies {
    api(projects.kobolIr)
    api(projects.kobolFir)

    api("app.softwork:sqldelight-writer:0.0.5")

    testImplementation(kotlin("test"))
}
