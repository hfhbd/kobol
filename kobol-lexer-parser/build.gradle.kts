plugins {
    id("org.jetbrains.grammarkit")
}

val idea = "221.5787.30"

grammarKit {
    intellijRelease.set(idea)
}

dependencies {
    compileOnly("com.jetbrains.intellij.java:java-psi:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:core-ui:$idea")
    compileOnly("com.jetbrains.intellij.platform:lang-impl:$idea")

    testImplementation(kotlin("test-junit"))
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:test-framework:$idea")
}

sourceSets["main"].java.srcDirs("$buildDir/generated/lexer/main/java", "$buildDir/generated/parser/main/java")

tasks {
    withType<JavaCompile> {
        sourceCompatibility = "11"
        targetCompatibility = "11"
    }
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = "11"
        dependsOn(generateParser, generateLexer)
    }

    generateLexer {
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.flex")
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/kobol/")
        targetClass.set("CobolLexer")
    }
    generateParser {
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.bnf")
        targetRoot.set("$buildDir/generated/parser/main/java")
        pathToParser.set("CobolParserGenerated.java")
        pathToPsiRoot.set("")
    }
}
