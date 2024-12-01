plugins {
    `kotlin-dsl`
    id("setup")
}

kotlin.jvmToolchain(21)

configurations.configureEach {
    if (isCanBeConsumed) {
        attributes {
            attribute(
                GradlePluginApiVersion.GRADLE_PLUGIN_API_VERSION_ATTRIBUTE,
                objects.named(GradleVersion.version("8.12").version)
            )
        }
    }
}

dependencies {
    compileOnly(projects.builder)

    testImplementation(kotlin("test"))
    testImplementation(projects.builder)
    testImplementation(projects.ir)
    testImplementation(libs.bundles.idea)
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

gradlePlugin{
    plugins.configureEach {
        displayName = "Kobol Gradle Plugin"
        description = "Kobol Gradle Plugin"
    }
    plugins.register("kobol") {
        id = "app.softwork.kobol"
        implementationClass = "app.softwork.kobol.gradle.KobolPlugin"
    }
    plugins.register("kobolSettings") {
        id = "app.softwork.kobol.settings"
        implementationClass = "app.softwork.kobol.gradle.KobolSettingsPlugin"
    }
}
