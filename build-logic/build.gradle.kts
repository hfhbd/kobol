plugins {
    `kotlin-dsl`
    kotlin("plugin.serialization") version embeddedKotlinVersion
}

dependencies {
    implementation(libs.plugins.kotlin.jvm.toDep())
    implementation(libs.plugins.kotlin.serialization.toDep())
    implementation(libs.plugins.publish.toDep())
    implementation(libs.plugins.licensee.toDep())
    implementation(libs.plugins.intellij.toDep())
    implementation(libs.plugins.shadow.toDep())
    implementation(libs.plugins.serviceloader.toDep())
    implementation(libs.plugins.ksp.toDep())

    implementation(libs.serialization.json)
    implementation(libs.datetime)
    implementation(libs.ktor.cio)
}

fun Provider<PluginDependency>.toDep() = map {
    "${it.pluginId}:${it.pluginId}.gradle.plugin:${it.version}"
}

kotlin.jvmToolchain(17)
