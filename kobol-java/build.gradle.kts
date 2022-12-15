plugins {
    setup
    repos
    intellijTesting
}

dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:javapoet:1.13.0")

    testImplementation(kotlin("test"))
}
