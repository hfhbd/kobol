plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.ir)
    implementation("com.squareup:kotlinpoet:1.12.0")

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(kotlin("test"))
    testImplementation(projects.kotlin.kotlinSqldelight)
    testImplementation(projects.kotlin.kotlinKotlinxserialization)
    testImplementation(projects.kotlin.kotlinFileJava)
    testImplementation(projects.plugins.pluginNullabletozero)
    testImplementation(projects.plugins.pluginOptimize)
    
    testImplementation(projects.plugins.pluginJavanames)
    testImplementation(projects.plugins.pluginKeepnames)
    testImplementation(projects.plugins.pluginInlining)
}

serviceLoaders.register("app.softwork.kobol.ir.CodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.generator.kotlin.KotlinCodeGeneratorFactory")
}
