dependencies {
    implementation(projects.kobolFir)

    val idea = "222.3739.54"
    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
