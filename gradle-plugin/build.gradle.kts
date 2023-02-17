plugins {
    `kotlin-dsl`
    kotlinSetup
}

dependencies {
    implementation(projects.ir)

    compileOnly(projects.kotlin.kotlinSqldelight)

    implementation("com.hierynomus:sshj:0.35.0")
    implementation("com.jcraft:jsch.agentproxy.sshj:0.0.9") // remove stupid open net.schmizz:sshj:[0.8.1,)
    implementation("com.jcraft:jsch.agentproxy.pageant:0.0.9")
    implementation("net.java.dev.jna:jna-platform:5.13.0")

    testImplementation(kotlin("test"))
    testImplementation(gradleTestKit())
    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(projects.plugins.pluginFlowGraphPlantuml)
    testImplementation(projects.kotlin)
    testImplementation(projects.java)
    testImplementation(projects.java.javaJava8)
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
