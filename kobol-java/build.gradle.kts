dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:javapoet:1.13.0")

    testImplementation(kotlin("test"))
    val idea = "222.4167.33"
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
