plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.psi)
    api(projects.fir)
    api(projects.ir)

    compileOnly(libs.bundles.idea)

    implementation(libs.db2.dialect) {
        exclude("com.alecstrong.sql.psi")
    }
    implementation("app.softwork.sql.psi:core:0.5.0-SNAPSHOT")
    testImplementation(libs.bundles.idea)
}
