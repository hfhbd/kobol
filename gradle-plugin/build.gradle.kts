plugins {
    `kotlin-dsl`
    id("setup")
}

kotlin.jvmToolchain(17)

configurations.configureEach {
    if (isCanBeConsumed) {
        attributes {
            attribute(
                GradlePluginApiVersion.GRADLE_PLUGIN_API_VERSION_ATTRIBUTE,
                objects.named<GradlePluginApiVersion>(GradleVersion.version("8.6").version)
            )
        }
    }
}

dependencies {
    compileOnly(projects.builder)
    compileOnly(projects.sshEnv)

    testImplementation(kotlin("test"))
    testImplementation(projects.builder)
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
sourceSets.main {
    kotlin.srcDir(storeVersion)
}

gradlePlugin.plugins.configureEach {
    displayName = "Kobol Gradle Plugin"
    description = "Kobol Gradle Plugin"
}
