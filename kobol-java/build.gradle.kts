plugins {
    setup
    repos
    intellijTesting
    app.softwork.serviceloader
}

dependencies {
    implementation(projects.kobolIr)
    implementation("com.squareup:javapoet:1.13.0")

    testImplementation(kotlin("test"))
    testImplementation(projects.kobolJavaJava8)
}

serviceLoaders.register("app.softwork.kobol.ir.CodeGeneratorFactory") {
    implementationClasses.add("app.softwork.kobol.generator.java.JavaCodeGeneratorFactory")
}
