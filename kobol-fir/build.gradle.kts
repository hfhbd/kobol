dependencies {
    api(projects.kobolLexerParser)

    val idea = "221.5787.30"
    implementation("com.jetbrains.intellij.java:java-psi:$idea")
    implementation("com.jetbrains.intellij.platform:core-impl:$idea")
    implementation("com.jetbrains.intellij.platform:core-ui:$idea")
    implementation("com.jetbrains.intellij.platform:lang-impl:$idea")

    testImplementation(kotlin("test-junit"))
}
