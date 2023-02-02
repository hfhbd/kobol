package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.initialization.*

public class KobolVersionPlugin : Plugin<Settings> {
    override fun apply(settings: Settings) {
        settings.dependencyResolutionManagement.versionCatalogs.register("kobol") {
            val version = version("kobol", kobolVersion)
            plugin("kobol", "app.softwork.kobol").versionRef(version)
            
            fun add(name: String, module: String) {
                library(name, "app.softwork", "kobol-$module").versionRef(version)   
            }
            
            fun plugin(module: String) {
                add(name = "plugin-$module", module = "plugins-$module")
            }

            add("kotlin", "kotlin")
            add("kotlin-sqldelight", "kotlin-sqldelight")
            add("kotlin-kotlinxserialization", "kotlin-kotlinxserialization")
            add("kotlin-file-java", "kotlin-file-java")
            
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
            plugin("ktor")
            plugin("nullabletozero")
            plugin("objects")
            plugin("optimize")
            plugin("private")
            plugin("readonlyvariables")
            plugin("useparameters")
        }
    }
}
