plugins {
    setup
    repos
    intellijTesting
}

dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:kotlinpoet:1.12.0")

    testImplementation(kotlin("test"))
    testImplementation(projects.kobolSqldelightPrecompiler)
    testImplementation(projects.kobolKotlinxSerialization)
    testImplementation(projects.kobolJavaFileKotlin)
    testImplementation(projects.kobolPlugins.kobolPluginsNullabletozero)
}
