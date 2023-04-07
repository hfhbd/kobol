plugins {
    setup
}

dependencies {
    implementation(projects.ir)
    implementation("com.squareup:kotlinpoet:1.13.0")

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
    testImplementation(projects.plugins.pluginNosynthetic)
    testImplementation(projects.plugins.pluginExitprocess)
}
