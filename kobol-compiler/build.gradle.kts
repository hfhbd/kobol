val idea = "221.5787.30"

dependencies {
    implementation(projects.kobolLexerParser)

    implementation("com.jetbrains.intellij.java:java-psi:$idea")
    implementation("com.jetbrains.intellij.platform:core-impl:$idea")
    implementation("com.jetbrains.intellij.platform:core-ui:$idea")
    implementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    implementation("com.squareup:kotlinpoet:1.11.0")

    testImplementation(kotlin("test-junit"))
}
