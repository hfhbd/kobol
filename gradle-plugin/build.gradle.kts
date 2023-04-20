plugins {
    `kotlin-dsl`
    kotlinSetup
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
    testImplementation(gradleTestKit())
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

java {
    withSourcesJar()
    withJavadocJar()
}

val storeVersion by tasks.registering(StoreVersion::class)
kotlin.sourceSets.main.configure {
    kotlin.srcDir(storeVersion)
}
