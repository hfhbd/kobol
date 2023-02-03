plugins {
    setup
    intellijTesting
    app.softwork.serviceloader
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
    
    testImplementation(projects.kobolPlugins.kobolPluginsJavanames)
    testImplementation(projects.kobolPlugins.kobolPluginsKeepnames)
}

serviceLoaders.register("app.softwork.kobol.ir.CodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.generator.kotlin.KotlinCodeGeneratorFactory")
}
