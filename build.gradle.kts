plugins {
    id("io.gitlab.arturbosch.detekt")
}

detekt {
    source.from(fileTree(layout.settingsDirectory) {
        include("**/*.kt")
        exclude("**/*.kts")
        exclude("**/resources/**")
        exclude("**/generated/**")
        exclude("**/build/**")
        exclude("util/src/main/kotlin/app/softwork/kobol/build.kt")
    })
    parallel = true
    autoCorrect = true
    buildUponDefaultConfig = true
    reports {
        sarif.required.set(true)
    }

    dependencies {
        detektPlugins("io.gitlab.arturbosch.detekt:detekt-formatting:${detekt.toolVersion}")
    }
}
