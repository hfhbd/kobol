dependencies {
    implementation(projects.kobolLexerParser)

    val idea = "211.7628.21"
    implementation("com.jetbrains.intellij.java:java-psi:$idea")
    implementation("com.jetbrains.intellij.platform:core-impl:$idea")
    implementation("com.jetbrains.intellij.platform:core-ui:$idea")
    implementation("com.jetbrains.intellij.platform:lang-impl:$idea")

    testImplementation(kotlin("test-junit"))
}
