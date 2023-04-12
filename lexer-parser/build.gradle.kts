plugins {
    setup
    repos
    org.jetbrains.grammarkit
}

grammarKit {
    val idea = "221.6008.13"
    intellijRelease.set(idea)
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.5.0")

    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shade"
    }

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }

    testImplementation(kotlin("test"))
    testImplementation(projects.fir)
}

sourceSets.main {
    java.srcDirs("$buildDir/generated/lexer/main/java", "$buildDir/generated/parser/main/java")
}

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
        dependsOn(generateParser, generateLexer)
    }

    generateLexer {
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.flex"))
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/kobol/")

        targetClass.set("CobolLexer") // Useless

        purgeOldFiles.set(true)
    }
    generateParser {
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.bnf"))
        targetRoot.set("$buildDir/generated/parser/main/java")
        
        pathToParser.set("CobolParserGenerated.java") // Useless
        pathToPsiRoot.set("") // Useless
        
        purgeOldFiles.set(true)
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
