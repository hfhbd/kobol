import org.gradle.kotlin.dsl.*

plugins {
    kotlin("jvm")
}

dependencies {
    testImplementation(kotlin("test"))

    val idea = "221.6008.13"
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea", excludeIntelliJ)
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea", excludeIntelliJ)
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea", excludeIntelliJ)
    testImplementation("com.jetbrains.intellij.platform:indexing-impl:$idea", excludeIntelliJ)
}
