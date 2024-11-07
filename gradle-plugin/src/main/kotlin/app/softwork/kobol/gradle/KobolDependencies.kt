package app.softwork.kobol.gradle

import org.gradle.api.artifacts.dsl.Dependencies
import org.gradle.api.artifacts.dsl.DependencyCollector

interface KobolDependencies : Dependencies {
    val compiler: DependencyCollector

    fun kotlin() = dependencyFactory.create("$GROUP:kotlin:$VERSION")
    fun kotlinSqldelight() = dependencyFactory.create("$GROUP:kotlin-sqldelight:$VERSION")
    fun kotlinKotlinxSerialization() = dependencyFactory.create("$GROUP:kotlin-kotlinxserialization:$VERSION")
    fun kotlinFileJava() = dependencyFactory.create("$GROUP:kotlin-file-java:$VERSION")

    fun java() = dependencyFactory.create("$GROUP:java:$VERSION")
    fun javaJava8() = dependencyFactory.create("$GROUP:java-java8:$VERSION")

    fun pluginBooleanexpressions() = dependencyFactory.create("$GROUP:plugin-booleanexpressions:$VERSION")
    fun pluginJavanames() = dependencyFactory.create("$GROUP:plugin-javanames:$VERSION")
    fun pluginKeepnames() = dependencyFactory.create("$GROUP:plugin-keepnames:$VERSION")
    fun pluginConstvariables() = dependencyFactory.create("$GROUP:plugin-constvariables:$VERSION")
    fun pluginFlowgraphplantuml() = dependencyFactory.create("$GROUP:plugin-flow-graph-plantuml:$VERSION")
    fun pluginIfassignments() = dependencyFactory.create("$GROUP:plugin-ifassignments:$VERSION")
    fun pluginInlining() = dependencyFactory.create("$GROUP:plugin-inlining:$VERSION")
    fun pluginMain() = dependencyFactory.create("$GROUP:plugin-main-util:$VERSION")
    fun pluginNosynthetic() = dependencyFactory.create("$GROUP:plugin-nosynthetic:$VERSION")
    fun pluginNullabletozero() = dependencyFactory.create("$GROUP:plugin-nullabletozero:$VERSION")
    fun pluginObjects() = dependencyFactory.create("$GROUP:plugin-objects:$VERSION")
    fun pluginOptimize() = dependencyFactory.create("$GROUP:plugin-optimize:$VERSION")
    fun pluginPrivate() = dependencyFactory.create("$GROUP:plugin-private:$VERSION")
    fun pluginRenaming() = dependencyFactory.create("$GROUP:plugin-renaming:$VERSION")
    fun pluginReadonlyvariables() = dependencyFactory.create("$GROUP:plugin-readonlyvariables:$VERSION")
    fun pluginStatistic() = dependencyFactory.create("$GROUP:plugin-statistic:$VERSION")
    fun pluginUseparameters() = dependencyFactory.create("$GROUP:plugin-useparameters:$VERSION")
    fun pluginExitProcess() = dependencyFactory.create("$GROUP:plugin-exitprocess:$VERSION")
}
