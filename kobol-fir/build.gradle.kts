dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.0")
    implementation(projects.kobolLexerParser)

    val idea = "222.4345.14"
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:project-model-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
