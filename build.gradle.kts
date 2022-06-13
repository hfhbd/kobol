plugins {
    kotlin("jvm") version "1.7.0" apply false
    id("org.jetbrains.intellij") version "1.6.0" apply false
    id("org.jetbrains.grammarkit") version "2021.2.2" apply false
    id("io.gitlab.arturbosch.detekt") version "1.20.0"
    `maven-publish`
}

group = "app.softwork"

allprojects {
    repositories {
        mavenCentral()
        maven(url = "https://www.jetbrains.com/intellij-repository/releases")
        maven(url = "https://cache-redirector.jetbrains.com/intellij-dependencies")
    }

    configurations.all {
        exclude(group = "com.jetbrains.rd")
        exclude(group = "com.github.jetbrains", module = "jetCheck")
        exclude(group = "org.roaringbitmap")
    }
}

detekt {
    source = files(rootProject.rootDir)
    parallel = true
    buildUponDefaultConfig = true
}

dependencies {
    detektPlugins("io.gitlab.arturbosch.detekt:detekt-formatting:1.20.0")
}

tasks {
    fun SourceTask.config() {
        include("**/*.kt")
        exclude("**/*.kts")
        exclude("**/resources/**")
        exclude("**/generated/**")
        exclude("**/build/**")
    }
    withType<io.gitlab.arturbosch.detekt.DetektCreateBaselineTask>().configureEach {
        config()
    }
    withType<io.gitlab.arturbosch.detekt.Detekt>().configureEach {
        config()

        reports {
            sarif.required.set(true)
        }
    }
}

subprojects {
    plugins.apply("maven-publish")
    plugins.apply("org.jetbrains.kotlin.jvm")

    publishing {
        repositories {
            maven(url = "https://maven.pkg.github.com/hfhbd/kobol") {
                name = "GitHubPackages"
                credentials {
                    username = System.getenv("GITHUB_ACTOR")
                    password = System.getenv("GITHUB_TOKEN")
                }
            }
        }
        publications.register<MavenPublication>("mavenJava") {
            from(components["java"])
        }
        publications.all {
            this as MavenPublication
            pom {
                name by "app.softwork KOBOL"
                url by "https://github.com/hfhbd/kobol"
                developers {
                    developer {
                        id by "hfhbd"
                        name by "Philip Wedemann"
                        email by "mybztg+mavencentral@icloud.com"
                    }
                }
                scm {
                    connection by "scm:git://github.com/hfhbd/kobol.git"
                    developerConnection by "scm:git://github.com/hfhbd/kobol.git"
                    url by "https://github.com/hfhbd/kobol"
                }
            }
        }
    }
}

infix fun <T> Property<T>.by(value: T) {
    set(value)
}
