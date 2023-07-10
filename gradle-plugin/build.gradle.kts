plugins {
    id("java-gradle-plugin")
    id("kotlinSetup")
}

configurations.configureEach {
    if (isCanBeConsumed) {
        attributes {
            attribute(
                GradlePluginApiVersion.GRADLE_PLUGIN_API_VERSION_ATTRIBUTE,
                objects.named<GradlePluginApiVersion>(GradleVersion.version("8.0").version)
            )
        }
    }
}

dependencies {
    implementation(projects.ir)
    compileOnly(projects.sshEnv)

    testImplementation(kotlin("test"))
    testImplementation(projects.ir)
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
    }
    testImplementation(projects.plugins.pluginFlowGraphPlantuml)
    testImplementation(projects.kotlin)
    testImplementation(projects.java)
    testImplementation(projects.java.javaJava8)
    testImplementation(projects.plugins.pluginNosynthetic)
}

tasks.validatePlugins {
    enableStricterValidation.set(true)
}

val storeVersion by tasks.registering(StoreVersion::class)
kotlin.sourceSets.main {
    kotlin.srcDir(storeVersion)
}

gradlePlugin.plugins.register("kobol") {
    id = "app.softwork.kobol"
    implementationClass = "app.softwork.kobol.gradle.KobolGradlePlugin"
    displayName = "Kobol Gradle Plugin"
    description = "Kobol Gradle Plugin"
}
