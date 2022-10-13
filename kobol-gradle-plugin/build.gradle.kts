plugins {
    `java-gradle-plugin`
    id("com.github.johnrengelman.shadow")
}

gradlePlugin {
    plugins {
        create("kobol") {
            id = "app.softwork.kobol"
            implementationClass = "app.softwork.kobol.KobolGradlePlugin"
        }
    }
}

val shade = configurations.create("shade")
configurations {
    compileOnly.get().extendsFrom(shade)
}

dependencies {
    implementation(projects.kobolKotlin)
    implementation(projects.kobolJava)
    implementation(projects.kobolFlowGraph)

    implementation("com.hierynomus:sshj:0.34.0")
    implementation("com.jcraft:jsch.agentproxy.sshj:0.0.9")
    implementation("com.jcraft:jsch.agentproxy.pageant:0.0.9")
    implementation("net.java.dev.jna:jna-platform:5.12.1")

    val idea = "222.4345.24"
    shade("com.jetbrains.intellij.platform:core-impl:$idea")
    shade("com.jetbrains.intellij.platform:project-model-impl:$idea")
    shade("com.jetbrains.intellij.platform:analysis-impl:$idea")

    testImplementation(kotlin("test"))
    testImplementation("com.jetbrains.intellij.platform:core-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:project-model-impl:$idea")
    testImplementation("com.jetbrains.intellij.platform:analysis-impl:$idea")
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

tasks.shadowJar {
    archiveClassifier.set("")
    configurations = listOf(shade)

    include("*.jar")
    include("misc/*.properties")
    include("app/softwork/kobol/**")

    include("org/intellij/**")
    include("com/intellij/**")
    include("org/picocontainer/**")
    include("it/unimi/**")
    include("org/jdom/**")
    include("com/github/benmanes/**")

    include("META-INF/gradle-plugins/*")
    include("messages/*.properties")

    exclude("/groovy**")
    exclude("/kotlin/**")
}

artifacts {
    runtimeOnly(tasks.shadowJar)
    archives(tasks.shadowJar)
}
