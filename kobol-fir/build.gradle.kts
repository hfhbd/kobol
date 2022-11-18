dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")
    implementation(projects.kobolLexerParser)

    implementation("com.alecstrong.sql.psi:core:0.42.42-SNAPSHOT")
    implementation("app.softwork:sqldelight-db2-dialect:0.0.2")

    val idea = "222.4459.20"
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:project-model-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
}
