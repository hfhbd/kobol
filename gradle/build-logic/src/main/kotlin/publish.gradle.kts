plugins {
    id("java-library")
    id("maven-publish")
    id("signing")
    id("io.github.hfhbd.mavencentral")
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
                    name.set("Apache-2.0")
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
    useInMemoryPgpKeys(
        project.providers.gradleProperty("signingKey").orNull,
        project.providers.gradleProperty("signingPassword").orNull,
    )
    isRequired = project.providers.gradleProperty("signingKey").isPresent
    sign(publishing.publications)
}
