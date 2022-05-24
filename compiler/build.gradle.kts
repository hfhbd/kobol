val idea = "221.5591.52"

dependencies {
    implementation(projects.lexerParser)

    compileOnly("com.jetbrains.intellij.java:java-psi:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-ui:$idea")
    compileOnly("com.jetbrains.intellij.platform:lang-impl:$idea")
    implementation("com.squareup:kotlinpoet:1.11.0")
}
