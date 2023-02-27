plugins {
    setup
    repos
    org.jetbrains.grammarkit
}

val idea = "221.6008.13"

grammarKit {
    intellijRelease.set(idea)
}

val grammar by configurations.registering {
    isCanBeResolved = true
    isCanBeConsumed = false
    isVisible = false
    defaultDependencies {
        add(project.dependencies.create("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4"))
    }
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.5.0")

    val idea = "221.6008.13"
    compileOnly("com.jetbrains.intellij.platform:util-ui:$idea")
    compileOnly(projects.intellijEnv) {
        targetConfiguration = "shade"
    }

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation("com.jetbrains.intellij.platform:util-ui:$idea")

    testImplementation(kotlin("test"))
    testImplementation(projects.fir)
}

sourceSets["main"].java.srcDirs("$buildDir/generated/lexer/main/java", "$buildDir/generated/parser/main/java")

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
        dependsOn(generateParser, generateLexer)
    }

    generateLexer {
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.flex"))
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/kobol/")
        targetClass.set("CobolLexer")
        purgeOldFiles.set(true)
    }
    generateParser {
        classpath(grammar)
        sourceFile.set(file("src/main/kotlin/app/softwork/kobol/Cobol.bnf"))
        targetRoot.set("$buildDir/generated/parser/main/java")
        pathToParser.set("CobolParserGenerated.java")
        pathToPsiRoot.set("")
        purgeOldFiles.set(true)
    }
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
