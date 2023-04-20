plugins {
    setup
    repos
    com.github.johnrengelman.shadow
}

val idea = "221.6008.13"
dependencies {
    shadow("com.jetbrains.intellij.platform:core-impl:$idea")
    shadow("com.jetbrains.intellij.platform:util-ui:$idea")
    shadow("com.jetbrains.intellij.platform:project-model-impl:$idea")
    shadow("com.jetbrains.intellij.platform:analysis-impl:$idea")
    shadow("com.jetbrains.intellij.platform:indexing-impl:$idea")
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

tasks.shadowJar {
    archiveClassifier.set("")
    dependsOn(tasks.jar)

    include("*.jar")
    include("misc/*.properties")

    include("org/intellij/**")
    include("com/intellij/**")
    include("org/picocontainer/**")
    include("it/unimi/**")
    include("org/jdom/**")
    include("com/github/benmanes/**")

    include("messages/*.properties")
    include("gnu/**")

    exclude("/kotlin/**")
}

val licenseeShadow by tasks.registering(app.cash.licensee.LicenseeTask::class) {
    configurationToCheck(configurations.shadow.get())
    outputDir.set(reporting.baseDirectory.dir("licenseeShadow"))
}

tasks.check {
    dependsOn(licenseeShadow)
}

afterEvaluate {
    tasks.named("licensee") {
        enabled = false
    }
}

licensee {
    allow("BSD-2-Clause")
    allowDependency("oro", "oro", "2.0.8") {
        because("Apache-2-0")
    }
    allowDependency("net.jcip", "jcip-annotations", "1.0") {
        because("Apache-2.0")
    }
    allowUrl("http://www.opensource.org/licenses/zlib-license.html") // BSD

    allowDependency("org.jetbrains.intellij.deps.fastutil", "intellij-deps-fastutil", "8.5.6-10") {
        because("Apache-2.0")
    }
    allowDependency("org.jetbrains.intellij.deps", "java-compatibility", "1.0.1") {
        because("Apache-2.0")
    }
    allowDependency("org.jetbrains.intellij.deps", "jdom", "2.0.6") {
        because("Apache-2.0")
    }
    allowUrl("https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html")

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
    allowDependency("com.jetbrains.intellij.platform", "object-serializer", idea)
    allowDependency("com.jetbrains.intellij.platform", "object-serializer-annotations", idea)
    allowDependency("com.jetbrains.intellij.platform", "project-model", idea)
    allowDependency("com.jetbrains.intellij.platform", "project-model-impl", idea)
    allowDependency("com.jetbrains.intellij.platform", "util", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-base", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-class-loader", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-diff", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-ex", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-rt", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-rt-java8", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-text-matching", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-ui", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-xml-dom", idea)
    allowDependency("com.jetbrains.intellij.platform", "util-zip", idea)
    allowDependency("com.jetbrains.intellij.platform", "workspace-model-storage", idea)
}
