import groovy.util.*

plugins {
    setup
    repos
    com.github.johnrengelman.shadow
}

val shade by configurations.register("shade")

configurations.implementation {
    extendsFrom(shade)
}

val idea = "231.8109.197"
dependencies {
    shade("com.jetbrains.intellij.platform:core-impl:$idea")
    shade("com.jetbrains.intellij.platform:project-model-impl:$idea")
    shade("com.jetbrains.intellij.platform:analysis-impl:$idea")
    shade("com.jetbrains.intellij.platform:indexing-impl:$idea")
    shade("com.jetbrains.intellij.platform:util-ui:$idea")
}

configurations.configureEach {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "com.github.jetbrains", module = "jetCheck")
    exclude(group = "com.jetbrains.intellij.platform", module = "wsl-impl")
    exclude(group = "com.jetbrains.intellij.platform", module = "diagnostic-telemetry")
    exclude(group = "com.jetbrains.intellij.platform", module = "statistics")
    exclude(group = "com.jetbrains.infra")
    exclude(group = "org.roaringbitmap")
    exclude(group = "ai.grazie.spell")
    exclude(group = "ai.grazie.model")
    exclude(group = "ai.grazie.utils")
    exclude(group = "ai.grazie.nlp")
    exclude(group = "jaxen")
    exclude(group = "com.h2database")
    exclude("com.jetbrains", "jbr-api")
}

tasks.shadowJar {
    archiveClassifier.set("")
    configurations = listOf(shade)

    include("*.jar")
    include("misc/*.properties")

    include("org/intellij/**")
    include("com/intellij/**")
    include("org/picocontainer/**")
    include("it/unimi/**")
    include("org/jdom/**")
    include("com/github/benmanes/**")

    include("messages/*.properties")
    include("kotlinx/**")
    include("gnu/**")

    exclude("/kotlin/**")
}

artifacts {
    runtimeOnly(tasks.shadowJar)
    archives(tasks.shadowJar)
}

licensee {
    allow("BSD-2-Clause")
    allow("BSD-3-Clause")
    allowDependency("oro", "oro", "2.0.8") {
        because("Apache-2-0")
    }
    allowDependency("net.jcip", "jcip-annotations", "1.0") {
        because("Apache-2.0")
    }
    allowUrl("http://www.opensource.org/licenses/zlib-license.html") // BSD

    allowDependency("org.jetbrains.intellij.deps.fastutil", "intellij-deps-fastutil", "8.5.11-18") {
        because("Apache-2.0")
    }
    allowDependency("org.jetbrains.intellij.deps", "java-compatibility", "1.0.1") {
        because("Apache-2.0")
    }

    allowDependency("com.jetbrains.intellij.platform", "analysis", idea)
    allowDependency("com.jetbrains.intellij.platform", "analysis-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "code-style", idea)
    allowDependency("com.jetbrains.intellij.platform", "concurrency", idea)
    allowDependency("com.jetbrains.intellij.platform", "core", idea)
    allowDependency("com.jetbrains.intellij.platform", "core-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "core-ui", idea)
    allowDependency("com.jetbrains.intellij.platform", "editor", idea)
    allowDependency("com.jetbrains.intellij.platform", "editor-ex", idea)
    allowDependency("com.jetbrains.intellij.platform", "extensions", idea)
    allowDependency("com.jetbrains.intellij.platform", "ide-util-io", idea)
    allowDependency("com.jetbrains.intellij.platform", "indexing", idea)
    allowDependency("com.jetbrains.intellij.platform", "indexing-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "jps-model", idea)
    allowDependency("com.jetbrains.intellij.platform", "jps-model-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "jps-model-serialization", idea)
    allowDependency("com.jetbrains.intellij.platform", "project-model", idea)
    allowDependency("com.jetbrains.intellij.platform", "project-model-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "util", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-base", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-class-loader", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-diff", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-ex", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-jdom", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-rt", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-rt-java8", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-text-matching", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-trove", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-ui", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-xml-dom", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-zip", idea)
    allowDependency("com.jetbrains.intellij.platform", "workspace-model-jps", idea)
    allowDependency("com.jetbrains.intellij.platform", "workspace-model-storage", idea)
}

// Disable Gradle module.json as it lists wrong dependencies
tasks.withType<GenerateModuleMetadata>().configureEach {
    enabled = false
}

// Remove dependencies from POM: uber jar has no dependencies
publishing {
    publications {
        named("mavenJava", MavenPublication::class) {
            artifacts.artifact(tasks.shadowJar)
            pom.withXml {
                val pomNode = asNode()

                val dependencyNodes = pomNode.get("dependencies") as NodeList
                dependencyNodes.forEach {
                    (it as Node).parent().remove(it)
                }
            }
        }
    }
}
