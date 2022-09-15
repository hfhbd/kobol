plugins {
    id("org.jetbrains.grammarkit")
}

val idea = "222.4167.29"

grammarKit {
    intellijRelease.set(idea)
}

val grammar = configurations.create("grammar") {
    isCanBeResolved = true
    isCanBeConsumed = false
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.0")
    grammar("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4")

    compileOnly("com.jetbrains.intellij.platform:core-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:util-ui:$idea")
    compileOnly("com.jetbrains.intellij.platform:project-model-impl:$idea")
    compileOnly("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(kotlin("test"))

    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:util-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(projects.kobolFir)
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
        classpath.from(grammar)
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.flex")
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/kobol/")
        targetClass.set("CobolLexer")
        purgeOldFiles.set(true)
    }
    generateParser {
        classpath.from(grammar)
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.bnf")
        targetRoot.set("$buildDir/generated/parser/main/java")
        pathToParser.set("CobolParserGenerated.java")
        pathToPsiRoot.set("")
        purgeOldFiles.set(true)
    }
}
