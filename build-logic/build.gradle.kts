plugins {
    `kotlin-dsl`
}

dependencies {
    implementation(libs.kotlin.gradlePlugin)
    implementation(libs.kotlin.serialization)
    implementation(libs.publish.gradlePlugin)
    implementation(libs.licensee.gradlePlugin)
    implementation(libs.intellij.gradlePlugin)
    implementation(libs.shadow.gradlePlugin)
    implementation(libs.serviceloader.gradlePlugin)
    implementation(libs.ksp.gradlePlugin)
}

kotlin.jvmToolchain(17)
