import app.cash.licensee.LicenseeTask

plugins {
    id("setup")
    id("repos")
    id("com.github.johnrengelman.shadow")
}

dependencies {
    shadow(libs.bundles.idea)
}

configurations.shadow {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "com.github.jetbrains", module = "jetCheck")
    exclude(group = "com.jetbrains.intellij.platform", module = "wsl-impl")
    exclude(group = "com.jetbrains.intellij.platform", module = "util-text-matching")
    exclude(group = "com.jetbrains.intellij.platform", module = "util-diff")
    exclude(group = "com.jetbrains.intellij.platform", module = "util-xml-dom")
    exclude(group = "com.jetbrains.intellij.platform", module = "util-zip")
    exclude(group = "com.jetbrains.intellij.platform", module = "code-style")
    exclude(group = "com.jetbrains.intellij.platform", module = "editor")
    exclude(group = "com.jetbrains.intellij.platform", module = "editor-ex")
    exclude(group = "com.jetbrains.intellij.platform", module = "jps-model-serialization")
    exclude(group = "com.jetbrains.intellij.platform", module = "object-serializer")
    exclude(group = "com.jetbrains.intellij.platform", module = "ide-util-io")
    exclude(group = "com.jetbrains.infra")
    exclude(group = "org.roaringbitmap")
    exclude(group = "ai.grazie.spell")
    exclude(group = "ai.grazie.model")
    exclude(group = "ai.grazie.utils")
    exclude(group = "ai.grazie.nlp")
    exclude(group = "oro")
    exclude(group = "org.lz4")
    exclude(group = "com.fasterxml")
    exclude(group = "be.cyberelf.nanoxml")
    exclude(group = "com.esotericsoftware.kryo")
    exclude(group = "dk.brics")
    exclude(group = "net.jcip")
    exclude(group = "org.jetbrains.intellij.deps", module = "trove4j")
    exclude(group = "org.jetbrains.intellij.deps", module = "java-compatibility")
    exclude(group = "org.jetbrains.intellij.deps.batik")
    exclude(group = "com.google.guava")
    exclude(group = "com.google.code.gson")
    exclude(group = "io.netty")
    exclude(group = "com.github.ben-manes.caffeine")
    exclude(group = "org.objenesis")
    exclude(group = "one.util", module = "streamex")
    exclude(group = "xml-apis")
    exclude(group = "org.imgscalr")
}

tasks.shadowJar {
    archiveClassifier.set("")
    configurations = listOf(project.configurations.shadow.get())

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

tasks.jar {
    enabled = false
}

configurations {
    apiElements {
        outgoing.artifacts.removeIf { tasks.jar.get() in it.buildDependencies.getDependencies(null) }
        outgoing.artifact(tasks.shadowJar)
    }
    runtimeElements {
        outgoing.artifacts.removeIf { tasks.jar.get() in it.buildDependencies.getDependencies(null) }
        outgoing.artifact(tasks.shadowJar)
    }
}

val licenseeShadow by tasks.registering(LicenseeTask::class) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
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
    allowDependency("org.jetbrains.intellij.deps.fastutil", "intellij-deps-fastutil", "8.5.6-10") {
        because("Apache-2.0")
    }
    allowDependency("org.jetbrains.intellij.deps", "jdom", "2.0.6") {
        because("Apache-2.0")
    }

    // https://youtrack.jetbrains.com/issue/IDEA-313536/IntelliJ-platform-artifacts-Add-license-information-to-pom-artifacts
    fun allowIdea(module: String) {
        allowDependency("com.jetbrains.intellij.platform", module, libs.versions.idea.get()) {
            because("Apache-2.0")
        }
    }

    allowIdea("analysis")
    allowIdea("analysis-impl")
    allowIdea("concurrency")
    allowIdea("core")
    allowIdea("core-impl")
    allowIdea("core-ui")
    allowIdea("extensions")
    allowIdea("indexing")
    allowIdea("indexing-impl")
    allowIdea("jps-model")
    allowIdea("jps-model-impl")
    allowIdea("project-model")
    allowIdea("project-model-impl")
    allowIdea("util")
    allowIdea("util-base")
    allowIdea("util-class-loader")
    allowIdea("util-ex")
    allowIdea("util-rt")
    allowIdea("util-rt-java8")
    allowIdea("util-ui")
    allowIdea("workspace-model-storage")
}

tasks.register("depsize", DepSize::class.java) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    configurationToCheck.from(configurations.shadow)
}
