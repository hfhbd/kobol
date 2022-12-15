import org.jetbrains.kotlin.gradle.tasks.*

plugins {
    setup
    repos
    `java-gradle-plugin`
    com.github.johnrengelman.shadow
    intellijTesting
}

gradlePlugin {
    plugins {
        create("kobol") {
            id = "app.softwork.kobol"
            implementationClass = "app.softwork.kobol.gradle.KobolGradlePlugin"
        }
    }
}

val shade by configurations.register("shade")
configurations {
    compileOnly.get().extendsFrom(shade)
}

dependencies {
    implementation(projects.kobolKotlin)
    implementation(projects.kobolIr)
    implementation(projects.kobolJava)
    implementation(projects.kobolFlowGraph)

    compileOnly(projects.kobolSqldelightPrecompiler)

    implementation("com.hierynomus:sshj:0.34.0")
    implementation("com.jcraft:jsch.agentproxy.sshj:0.0.9") // remove stupid open net.schmizz:sshj:[0.8.1,)
    implementation("com.jcraft:jsch.agentproxy.pageant:0.0.9")
    implementation("net.java.dev.jna:jna-platform:5.12.1")

    val idea = "222.4459.24"
    shade("com.jetbrains.intellij.platform:core:$idea")
    shade("com.jetbrains.intellij.platform:project-model:$idea")
    shade("com.jetbrains.intellij.platform:analysis:$idea")
    shade("com.jetbrains.intellij.platform:indexing:$idea")

    testImplementation(gradleTestKit())
    testImplementation(projects.kobolPlugins.kobolPluginsFlowGraphPlantuml)
}

tasks.withType<KotlinCompile>().configureEach {
    kotlinOptions.apiVersion = "1.4"
}

licensee {
    allow("MIT")
    allowUrl("http://www.jcraft.com/jsch-agent-proxy/LICENSE.txt") // BSD
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") // BSD
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allowUrl("https://creativecommons.org/publicdomain/zero/1.0/")
}

tasks.validatePlugins {
    enableStricterValidation.set(true)
}

tasks.shadowJar {
    archiveClassifier.set("")
    configurations = listOf(shade)

    include("*.jar")
    include("misc/*.properties")
    include("app/softwork/kobol/**")

    include("org/intellij/**")
    include("com/intellij/**")
    include("org/picocontainer/**")
    include("it/unimi/**")
    include("org/jdom/**")
    include("com/github/benmanes/**")

    include("META-INF/gradle-plugins/*")
    include("messages/*.properties")

    exclude("/groovy**")
    exclude("/kotlin/**")
}

artifacts {
    runtimeOnly(tasks.shadowJar)
    archives(tasks.shadowJar)
}

java {
    withSourcesJar()
    withJavadocJar()
}

tasks {
    val storeVersion by registering {
        val version = project.version
        val outputDirGenerated = project.layout.buildDirectory.dir("generated/kobol")
        val outputDir = outputDirGenerated.map { it.dir("app/softwork/kobol/gradle") }
        val outputFile = outputDir.map { it.file("Version.kt") }

        kotlin.sourceSets.main.configure {
            kotlin.srcDir(outputDirGenerated)
        }

        doLast {
            outputDir.get().asFile.mkdirs()

            outputFile.get().asFile.writeText(
                """
                package app.softwork.kobol.gradle
                
                internal val kobolVersion = "$version"
                
                """.trimIndent()
            )
        }
    }

    compileKotlin {
        dependsOn(storeVersion)
    }
}
