dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:kotlinpoet:1.12.0")

    testImplementation(kotlin("test"))
    val idea = "222.4345.14"
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
