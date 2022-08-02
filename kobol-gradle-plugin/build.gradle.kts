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
    implementation("net.java.dev.jna:jna-platform:5.12.1")

    val idea = "211.7628.21"
    implementation("com.jetbrains.intellij.java:java-psi:$idea")
    implementation("com.jetbrains.intellij.platform:core-impl:$idea")
    implementation("com.jetbrains.intellij.platform:core-ui:$idea")
    implementation("com.jetbrains.intellij.platform:lang-impl:$idea")
    implementation("com.jetbrains.intellij.platform:test-framework:$idea")

    testImplementation(kotlin("test"))
}

licensee {
    allow("MIT")
    allowUrl("http://www.jcraft.com/jsch-agent-proxy/LICENSE.txt") // BSD
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") // BSD
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allowUrl("https://creativecommons.org/publicdomain/zero/1.0/")
}
