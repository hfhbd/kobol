import org.gradle.kotlin.dsl.*

plugins {
    kotlin("jvm")
}

dependencies {
    val idea = "222.4459.24"
    compileOnly("com.jetbrains.intellij.platform:ide-impl:$idea")
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