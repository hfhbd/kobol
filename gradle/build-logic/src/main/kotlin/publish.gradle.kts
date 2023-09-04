import java.util.*

plugins {
    id("java")
    id("maven-publish")
    id("signing")
}

java {
    withSourcesJar()
    withJavadocJar()
}

publishing {
    publications.withType<MavenPublication>().configureEach {
        pom {
            name.set("app.softwork KOBOL")
            description.set("A Cobol to Kotlin converter")
            url.set("https://github.com/hfhbd/kobol")
            licenses {
                license {
                    name.set("The Apache License, Version 2.0")
                    url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
                }
            }
            developers {
                developer {
                    id.set("hfhbd")
                    name.set("Philip Wedemann")
                    email.set("mybztg+mavencentral@icloud.com")
                }
            }
            scm {
                connection.set("scm:git://github.com/hfhbd/kobol.git")
                developerConnection.set("scm:git://github.com/hfhbd/kobol.git")
                url.set("https://github.com/hfhbd/kobol")
            }
        }
    }
}

signing {
    val signingKey: String? by project
    val signingPassword: String? by project
    signingKey?.let {
        useInMemoryPgpKeys(String(Base64.getDecoder().decode(it)).trim(), signingPassword)
        sign(publishing.publications)
    }
}

// https://youtrack.jetbrains.com/issue/KT-46466
val signingTasks = tasks.withType<Sign>()
tasks.withType<AbstractPublishToMaven>().configureEach {
    dependsOn(signingTasks)
}

tasks.withType<AbstractArchiveTask>().configureEach {
    isPreserveFileTimestamps = false
    isReproducibleFileOrder = true
}
