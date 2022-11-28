plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
    `maven-publish`
    app.cash.licensee
}

licensee {
    allow("Apache-2.0")
}

configurations.all {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "com.github.jetbrains", module = "jetCheck")
    exclude(group = "com.jetbrains.infra")
    exclude(group = "org.roaringbitmap")
    exclude(group = "ai.grazie.spell")
    exclude(group = "ai.grazie.utils")
    exclude(group = "ai.grazie.nlp")
}

tasks {
    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "17"
        targetCompatibility = "17"
    }
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions {
            jvmTarget = "17"
        }
    }
}

kotlin {
    explicitApi()
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

infix fun <T> Property<T>.by(value: T) {
    set(value)
}
