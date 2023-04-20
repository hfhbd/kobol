plugins {
    setup
}

dependencies {
    api("org.jetbrains.kotlinx:kotlinx-serialization-core:1.5.0")
    implementation(projects.lexerParser)
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    implementation("app.softwork:sqldelight-db2-dialect:0.0.3-SNAPSHOT")
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
}
