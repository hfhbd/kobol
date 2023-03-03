import app.softwork.kobol.gradle.*

dependencyResolutionManagement.versionCatalogs.register("kobol") {
    val version = version("kobol", kobolVersion)
    plugin("kobol", "app.softwork.kobol").versionRef(version)

    fun add(name: String, module: String) {
        library(name, "app.softwork.kobol", module).versionRef(version)
    }

    fun plugin(module: String) {
        add(name = "plugin-$module", module = "plugin-$module")
    }

    add("kotlin", "kotlin")
    add("kotlin-sqldelight", "kotlin-sqldelight")
    add("kotlin-kotlinxserialization", "kotlin-kotlinxserialization")
    add("kotlin-filejava", "kotlin-file-java")

    add("java", "java")
    add("java-java8", "java-java8")
    add("java-jdbc", "java-jdbc")

    plugin("booleanexpressions")
    plugin("javanames")
    plugin("keepnames")
    plugin("constvariables")
    plugin("flow-graph-plantuml")
    plugin("ifassignments")
    plugin("inlining")
    plugin("nullabletozero")
    plugin("objects")
    plugin("optimize")
    plugin("private")
    plugin("readonlyvariables")
    plugin("statistic")
    plugin("useparameters")
    plugin("exitprocess")
    plugin("nosynthetic")
}
