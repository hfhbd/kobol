import org.jetbrains.kotlin.gradle.dsl.*

plugins {
    app.cash.licensee
    `maven-publish`
}

publishing {
    repositories {
        maven(url = "https://maven.pkg.github.com/hfhbd/kobol") {
            name = "GitHubPackages"
            credentials(PasswordCredentials::class)
        }
    }
    if (name != "kobol-gradle-plugin") {
        publications.register<MavenPublication>("mavenJava") {
            from(components["java"])
        }
    }
    publications.configureEach {
        this as MavenPublication
        pom {
            name.set("app.softwork KOBOL")
            url.set("https://github.com/hfhbd/kobol")
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

pluginManager.withPlugin("org.jetbrains.kotlin.jvm") {
    extensions.configure<KotlinJvmProjectExtension>("kotlin") {
        jvmToolchain(17)
        explicitApi()
        target {
            compilations.configureEach {
                kotlinSourceSets.forAll {
                    it.languageSettings.progressiveMode = true
                }
                kotlinOptions {
                    // allWarningsAsErrors = true
                }
            }
        }
    }
}

licensee {
    allow("Apache-2.0")
}
