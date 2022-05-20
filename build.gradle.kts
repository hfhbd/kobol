plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "1.6.21"
    id("org.jetbrains.intellij") version "1.5.3"
    id("org.jetbrains.grammarkit") version "2021.2.2"
}

group = "app.softwork"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

// Configure Gradle IntelliJ Plugin - read more: https://github.com/JetBrains/gradle-intellij-plugin
intellij {
    version.set("2022.1")
    type.set("IU") // Target IDE Platform
}

tasks {
    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "11"
        targetCompatibility = "11"
    }
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = "11"
    }

    patchPluginXml {
        sinceBuild.set("221")
        untilBuild.set("222.*")
    }

    signPlugin {
        certificateChain.set(System.getenv("CERTIFICATE_CHAIN"))
        privateKey.set(System.getenv("PRIVATE_KEY"))
        password.set(System.getenv("PRIVATE_KEY_PASSWORD"))
    }

    publishPlugin {
        token.set(System.getenv("PUBLISH_TOKEN"))
    }
}
sourceSets["main"].java.srcDirs("$buildDir/generated/lexer/main/java", "$buildDir/generated/parser/main/java")

tasks {
    generateLexer {
        source.set("src/main/kotlin/app/softwork/cobolidea/Cobol.flex")
        targetDir.set("$buildDir/generated/lexer/main/java/app/softwork/cobolidea/")
        targetClass.set("CobolLexer")
    }
    generateParser {
        source.set("src/main/kotlin/app/softwork/cobolidea/Cobol.bnf")
        targetRoot.set("$buildDir/generated/parser/main/java")
        pathToParser.set("CobolParserGenerated.java")
        pathToPsiRoot.set("")
    }
    assemble {
        dependsOn(generateLexer, generateParser)
    }
}
