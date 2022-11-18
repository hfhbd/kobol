dependencies {
    implementation(projects.kobolFir)

    val idea = "222.4459.20"
    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
