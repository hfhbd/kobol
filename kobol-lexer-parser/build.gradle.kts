import gradle.kotlin.dsl.accessors._5b841d749b44c4d634f7cfea3ed45134.*

plugins {
    setup
    repos
    org.jetbrains.grammarkit
    intellij
    intellijTesting
}

val idea = "221.6008.13"

grammarKit {
    intellijRelease.set(idea)
}

val grammar by configurations.registering {
    isCanBeResolved = true
    isCanBeConsumed = false
    defaultDependencies {
        add(project.dependencies.create("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4"))
    }
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:1.4.1")

    val idea = "221.6008.13"
    compileOnly("com.jetbrains.intellij.platform:util-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:util-ui:$idea")

    testImplementation(projects.kobolFir)
}

sourceSets["main"].java.srcDirs("$buildDir/generated/lexer/main/java", "$buildDir/generated/parser/main/java")

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
        dependsOn(generateParser, generateLexer)
    }

    generateLexer {
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.flex")
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/kobol/")
        targetClass.set("CobolLexer")
        purgeOldFiles.set(true)
    }
    generateParser {
        classpath(grammar)
        source.set("$projectDir/src/main/kotlin/app/softwork/kobol/Cobol.bnf")
        targetRoot.set("$buildDir/generated/parser/main/java")
        pathToParser.set("CobolParserGenerated.java")
        pathToPsiRoot.set("")
        purgeOldFiles.set(true)
    }
}
