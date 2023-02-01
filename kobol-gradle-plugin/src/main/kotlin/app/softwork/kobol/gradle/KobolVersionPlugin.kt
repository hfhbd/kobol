package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.initialization.*

public class KobolVersionPlugin : Plugin<Settings> {
    override fun apply(settings: Settings) {
        settings.dependencyResolutionManagement.versionCatalogs.register("kobol") {
            val version = version("kobol", kobolVersion)
            plugin("kobol", "app.softwork.kobol").versionRef(version)
            
            fun add(module: String) {
                library(module, "app.softwork", "kobol-$module").versionRef(version)   
            }
            
            fun plugin(module: String) {
                add("plugins-$module")
            }

            add("kotlin")
            add("kotlin-sqldelight")
            add("kotlin-kotlinxserialization")
            add("kotlin-file-java")
            
            add("java")
            add("java-java8")
            add("java-jdbc")

            plugin("booleanexpressions")
            plugin("camelcase")
            // plugin("classes")
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
