dependencies {
    implementation(projects.kobolIr)
    implementation(projects.kobolFir)

    implementation("app.softwork:sqldelight-writer:0.0.5")

    testImplementation(kotlin("test"))
}
