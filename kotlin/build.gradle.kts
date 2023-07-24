plugins {
    id("kotlinSetup")
}

dependencies {
    implementation(projects.ir)
    implementation(libs.kotlinpoet)

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shadow"
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
    testImplementation(projects.plugins.pluginMainUtil)
}
