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

    testImplementation(kotlin("test"))
}
