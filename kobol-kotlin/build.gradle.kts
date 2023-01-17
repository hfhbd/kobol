plugins {
    setup
    repos
    intellijTesting
}

dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:kotlinpoet:1.12.0")

    testImplementation(kotlin("test"))
    testImplementation(projects.kobolKotlinSqldelight)
    testImplementation(projects.kobolKotlinKotlinxserialization)
    testImplementation(projects.kobolKotlinFileJava)
    testImplementation(projects.kobolPlugins.kobolPluginsNullabletozero)
    testImplementation(projects.kobolPlugins.kobolPluginsOptimize)
}
