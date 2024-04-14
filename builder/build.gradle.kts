plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.psi)
    api(projects.fir)
    api(projects.ir)

    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    implementation(libs.db2.dialect)
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
}
