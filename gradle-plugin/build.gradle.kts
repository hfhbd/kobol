plugins {
    `kotlin-dsl`
    kotlinSetup
}

val addMinGradleVersionAttribute: Configuration.() -> Unit = {
    attributes {
        attribute(
            GradlePluginApiVersion.GRADLE_PLUGIN_API_VERSION_ATTRIBUTE,
            objects.named<GradlePluginApiVersion>("8.0")
        )
    }
}

configurations.apiElements(addMinGradleVersionAttribute)
configurations.runtimeElements(addMinGradleVersionAttribute)

dependencies {
    implementation(projects.ir)
    compileOnly(projects.sshEnv)

    testImplementation(kotlin("test"))
    testImplementation(gradleTestKit())
    testImplementation(projects.ir)
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
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

tasks {
    val storeVersion by registering(StoreVersion::class) {
        kotlin.sourceSets.main.configure {
            kotlin.srcDir(generated)
        }
    }

    compileKotlin {
        dependsOn(storeVersion)
    }
}
