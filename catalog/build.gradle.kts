plugins {
    `version-catalog`
    publish
}

publishing {
    publications {
        register<MavenPublication>("maven") {
            from(components["versionCatalog"])
        }
    }
}

catalog {
    versionCatalog {
        val version = version("kobol", project.version.toString())
        plugin("kobol", "app.softwork.kobol").versionRef(version)

        fun add(name: String, module: String = name) {
            library(name, "app.softwork.kobol", module).versionRef(version)
        }

        fun plugin(module: String) {
            add(name = "plugin-$module", module = "plugin-$module")
        }

        add("f-3.1")

        add("ir")
        add("fir")

        add("kotlin")
        add("kotlin-sqldelight")
        add("kotlin-kotlinxserialization")
        add("kotlin-filejava", "kotlin-file-java")

        add("java")
        add("java-java8")
        add("java-jdbc")

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
}
