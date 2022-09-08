dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:kotlinpoet:1.12.0")

    testImplementation(kotlin("test"))
    val idea = "222.3739.54"
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:test-framework:$idea")
}
