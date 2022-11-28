plugins {
    setup
}

dependencies {
    implementation(projects.kobolFlowGraph)

    val idea = "222.4459.24"
    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
