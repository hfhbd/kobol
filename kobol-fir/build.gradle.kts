dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.0")
    implementation(projects.kobolLexerParser)

    val idea = "222.3739.54"
    compileOnly("com.jetbrains.intellij.java:java-psi:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-ui:$idea")
    compileOnly("com.jetbrains.intellij.platform:project-model:$idea")
    compileOnly("com.jetbrains.intellij.platform:project-model-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
