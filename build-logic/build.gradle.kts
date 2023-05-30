plugins {
    `kotlin-dsl`
    kotlin("plugin.serialization") version embeddedKotlinVersion
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
    
    implementation(libs.serialization.json)
    implementation(libs.datetime)
    implementation(libs.ktor.cio)
}

kotlin.jvmToolchain(17)
