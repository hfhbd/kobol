plugins {
    id("kotlinSetup")
    id("java-test-fixtures")
}

dependencies {
    implementation(projects.ir)
    implementation(libs.javapoet)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.builder)
    testImplementation(kotlin("test"))
    testImplementation(projects.java.javaJava8)
    testImplementation(projects.plugins.pluginNosynthetic)
    testImplementation(projects.plugins.pluginExitprocess)
}
