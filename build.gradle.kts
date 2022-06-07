import io.gitlab.arturbosch.detekt.*

plugins {
    kotlin("jvm") version "1.6.21" apply false
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
