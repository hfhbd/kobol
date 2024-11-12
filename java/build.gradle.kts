plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.ir)
    implementation(libs.javapoet)

    testImplementation(libs.bundles.idea)
    testImplementation(projects.builder)
    testImplementation(projects.java.javaJava8)
    testImplementation(projects.plugins.pluginNosynthetic)
    testImplementation(projects.plugins.pluginExitprocess)
}
