import gradle.kotlin.dsl.accessors._b6c1ee95cd8c9d49d26ad7df1bd5c889.*
import org.gradle.kotlin.dsl.*

plugins {
    kotlin("jvm")
}

dependencies {
    testImplementation(kotlin("test"))

    val idea = "221.6008.13"
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:indexing-impl:$idea")
}

configurations.configureEach {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "com.github.jetbrains", module = "jetCheck")
    exclude(group = "com.jetbrains.infra")
    exclude(group = "org.roaringbitmap")
    exclude(group = "ai.grazie.spell")
    exclude(group = "ai.grazie.model")
    exclude(group = "ai.grazie.utils")
    exclude(group = "ai.grazie.nlp")
}
