plugins {
    setup
    app.softwork.serviceloader
}

dependencies {
    api(projects.ir)
    implementation("com.squareup:javapoet:1.13.0")

    testImplementation(projects.intellijEnv) {
        targetConfiguration = "shade"
    }
    testImplementation(kotlin("test"))
    testImplementation(projects.java.javaJava8)
}

serviceLoaders.register("app.softwork.kobol.ir.CodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.generator.java.JavaCodeGeneratorFactory")
}
