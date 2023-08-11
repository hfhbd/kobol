plugins {
    id("kotlinSetup")
}

dependencies {
    api(libs.serialization.core)
    implementation(projects.lexerParser)
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    implementation(libs.db2.dialect) {
        exclude("com.alecstrong.sql.psi")
    }
    implementation("app.softwork.sql.psi:core:0.5.0-SNAPSHOT")
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
}
