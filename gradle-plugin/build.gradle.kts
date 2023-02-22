plugins {
    `kotlin-dsl`
    kotlinSetup
}

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
