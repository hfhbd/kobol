plugins {
    setup
}

dependencies {
    api("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4")
    api("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")
    implementation(projects.lexerParser)
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shade"
    }

    implementation("app.softwork:sqldelight-db2-dialect:0.0.3-SNAPSHOT")
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
}
