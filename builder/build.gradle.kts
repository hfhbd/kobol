plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.psi)
    api(projects.fir)
    api(projects.ir)

    compileOnly(libs.bundles.idea)

    implementation(libs.db2.dialect)
    implementation(libs.sql.psi.environment)
    testImplementation(libs.bundles.idea)
}
