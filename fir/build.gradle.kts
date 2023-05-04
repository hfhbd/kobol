plugins {
    id("setup")
}

dependencies {
    api(libs.serialization.core)
    implementation(projects.lexerParser)
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    implementation(libs.db2.dialect)
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
}
