val idea = "211.7628.21"

dependencies {
    implementation(projects.lexerParser)

    implementation("com.jetbrains.intellij.java:java-psi:$idea")
    implementation("com.jetbrains.intellij.platform:core-impl:$idea")
    implementation("com.jetbrains.intellij.platform:core-ui:$idea")
    implementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    implementation("com.squareup:kotlinpoet:1.11.0")

    testImplementation(kotlin("test-junit"))
}
