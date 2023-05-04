plugins {
    id("setup")
}

dependencies {
    implementation(projects.ir)
    implementation("com.squareup:javapoet:1.13.0")

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(kotlin("test"))
    testImplementation(projects.java.javaJava8)
    testImplementation(projects.plugins.pluginNosynthetic)
    testImplementation(projects.plugins.pluginExitprocess)
}
