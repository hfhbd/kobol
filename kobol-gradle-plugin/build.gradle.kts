plugins {
    `java-gradle-plugin`
}

gradlePlugin {
    plugins {
        create("kobol") {
            id = "app.softwork.kobol"
            implementationClass = "app.softwork.kobol.KobolGradlePlugin"
        }
    }
}

dependencies {
    implementation(projects.kobolKotlin)

    api("com.hierynomus:sshj:0.33.0")
    implementation("com.jcraft:jsch.agentproxy.sshj:0.0.9")
    implementation("com.jcraft:jsch.agentproxy.pageant:0.0.9")

    testImplementation(kotlin("test"))
    val idea = "211.7628.21"
    testImplementation("com.jetbrains.intellij.java:java-psi:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:core-ui:$idea")
    testImplementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:test-framework:$idea")
}

licensee {
    allow("MIT")
    allowUrl("http://www.jcraft.com/jsch-agent-proxy/LICENSE.txt") // BSD
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") // BSD
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allowUrl("https://creativecommons.org/publicdomain/zero/1.0/")
}
