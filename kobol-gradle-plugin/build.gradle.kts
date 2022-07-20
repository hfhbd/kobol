plugins {
    `java-gradle-plugin`
}

gradlePlugin {
    plugins {
        create("kobol") {
            id = "app.softwork.kobol"
            implementationClass = "app.softwork.kobol.KobolGradlePlugin"
        }
    }
}

dependencies {
    implementation(projects.kobolKotlin)

    testImplementation(kotlin("test"))
    val idea = "221.6008.13"
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:test-framework:$idea")
}
