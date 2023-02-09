package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.initialization.*

public class KobolVersionPlugin : Plugin<Settings> {
    override fun apply(settings: Settings) {
        settings.dependencyResolutionManagement.versionCatalogs.register("kobol") {
            val version = version("kobol", kobolVersion)
            for ((name, notation) in plugins) {
                plugin(name, notation).versionRef(version)
            }
            for (name in deps) {
                library(name, "app.softwork.kobol", name).versionRef(version)
            }
        }
    }

    internal companion object {
        val plugins = listOf("kobol" to "app.softwork.kobol")

        val deps = listOf(
            "kotlin",
            "kotlin-sqldelight",
            "kotlin-kotlinxserialization",
            "kotlin-file-java",

            "java",
            "java-java8",
            "java-jdbc",

            "plugin-booleanexpressions",
            "plugin-javanames",
            "plugin-keepnames",
            "plugin-constvariables",
            "plugin-flow-graph-plantuml",
            "plugin-ifassignments",
            "plugin-inlining",
            "plugin-ktor",
            "plugin-nullabletozero",
            "plugin-objects",
            "plugin-optimize",
            "plugin-private",
            "plugin-readonlyvariables",
            "plugin-useparameters",
        )
    }
}
