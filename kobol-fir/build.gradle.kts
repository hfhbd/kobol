plugins {
    setup
    intellij
    intellijTesting
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")
    implementation(projects.kobolLexerParser)

    implementation("app.softwork:sqldelight-db2-dialect:0.0.3-SNAPSHOT")
}
