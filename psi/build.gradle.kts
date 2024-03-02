plugins {
    id("kotlinSetup")
    id("org.jetbrains.grammarkit")
}

grammarKit {
    intellijRelease.set(libs.versions.idea)
}

dependencies {
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }

    testImplementation(kotlin("test"))
    testImplementation(projects.builder)
}

sourceSets.main {
    java.srcDirs(tasks.generateLexer, tasks.generateParser)
}

tasks {
    generateLexer {
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.flex"))
        targetOutputDir.set(layout.buildDirectory.dir("generated/lexer/main/java/app/softwork/kobol/"))
    }
    generateParser {
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.bnf"))
        targetRootOutputDir.set(layout.buildDirectory.dir("generated/parser/main/java"))
    }
}

configurations.configureEach {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "com.github.jetbrains", module = "jetCheck")
    exclude(group = "com.jetbrains.intellij.platform", module = "wsl-impl")
    exclude(group = "com.jetbrains.infra")
    exclude(group = "org.roaringbitmap")
    exclude(group = "ai.grazie.spell")
    exclude(group = "ai.grazie.model")
    exclude(group = "ai.grazie.utils")
    exclude(group = "ai.grazie.nlp")
}
