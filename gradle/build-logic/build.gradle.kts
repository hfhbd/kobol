plugins {
    `kotlin-dsl`
}

dependencies {
    implementation(libs.plugins.kotlin.jvm.toDep())
    implementation(libs.plugins.kotlin.serialization.toDep())
    implementation(libs.plugins.licensee.toDep())
    implementation(libs.plugins.intellij.toDep())
    implementation(libs.plugins.shadow.toDep())
    implementation(libs.plugins.serviceloader.toDep())
    implementation(libs.plugins.detekt.toDep())
}

fun Provider<PluginDependency>.toDep() = map {
    "${it.pluginId}:${it.pluginId}.gradle.plugin:${it.version}"
}
