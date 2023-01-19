import org.gradle.kotlin.dsl.*

plugins {
    kotlin("jvm")
}

dependencies {
    val idea = "221.6008.13"
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea", excludeIntelliJ)
    compileOnly("com.jetbrains.intellij.platform:project-model-impl:$idea", excludeIntelliJ)
    compileOnly("com.jetbrains.intellij.platform:analysis-impl:$idea", excludeIntelliJ)
    compileOnly("com.jetbrains.intellij.platform:indexing-impl:$idea", excludeIntelliJ)
}
