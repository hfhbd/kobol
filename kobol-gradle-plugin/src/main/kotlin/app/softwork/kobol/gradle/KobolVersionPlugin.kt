package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.initialization.*

public class KobolVersionPlugin : Plugin<Settings> {
    override fun apply(settings: Settings) {
        settings.dependencyResolutionManagement.versionCatalogs.register("kobol") {
            val version = it.version("kobol", kobolVersion)
            it.plugin("kobol", "app.softwork.kobol").versionRef(version)
            
            fun add(module: String) {
                it.library("kobol-$module", "app.softwork.kobol", "kobol-$module").versionRef(version)   
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

            plugin("booleanExpressions")
            plugin("camelCase")
            // plugin("classes")
            plugin("constVariables")
            plugin("flow-graph-plantuml")
            plugin("ifAssignments")
            plugin("ktor")
            plugin("nullabletozero")
            plugin("objects")
            plugin("optimize")
            plugin("private")
            plugin("readOnlyVariables")
            plugin("useParameters")
        }
    }
}
