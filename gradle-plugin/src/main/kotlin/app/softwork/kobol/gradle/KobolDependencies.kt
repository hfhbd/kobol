package app.softwork.kobol.gradle

import org.gradle.api.artifacts.ExternalModuleDependency
import org.gradle.api.artifacts.dsl.Dependencies
import org.gradle.api.artifacts.dsl.DependencyCollector

public interface KobolDependencies : Dependencies {
    public val compiler: DependencyCollector

    public fun kotlin(): ExternalModuleDependency = dependencyFactory.create("$GROUP:kotlin:$VERSION")
    public fun kotlinSqldelight(): ExternalModuleDependency = dependencyFactory.create("$GROUP:kotlin-sqldelight:$VERSION")
    public fun kotlinKotlinxSerialization(): ExternalModuleDependency = dependencyFactory.create("$GROUP:kotlin-kotlinxserialization:$VERSION")
    public fun kotlinFileJava(): ExternalModuleDependency= dependencyFactory.create("$GROUP:kotlin-file-java:$VERSION")

    public fun java(): ExternalModuleDependency = dependencyFactory.create("$GROUP:java:$VERSION")
    public fun javaJava8(): ExternalModuleDependency = dependencyFactory.create("$GROUP:java-java8:$VERSION")

    public fun pluginBooleanexpressions(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-booleanexpressions:$VERSION")
    public fun pluginJavanames() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-javanames:$VERSION")
    public fun pluginKeepnames() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-keepnames:$VERSION")
    public fun pluginConstvariables(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-constvariables:$VERSION")
    public fun pluginFlowgraphplantuml(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-flow-graph-plantuml:$VERSION")
    public fun pluginIfassignments() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-ifassignments:$VERSION")
    public fun pluginInlining() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-inlining:$VERSION")
    public fun pluginMain() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-main-util:$VERSION")
    public fun pluginNosynthetic(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-nosynthetic:$VERSION")
    public fun pluginNullabletozero(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-nullabletozero:$VERSION")
    public fun pluginObjects() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-objects:$VERSION")
    public fun pluginOptimize(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-optimize:$VERSION")
    public fun pluginPrivate() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-private:$VERSION")
    public fun pluginRenaming(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-renaming:$VERSION")
    public fun pluginReadonlyvariables(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-readonlyvariables:$VERSION")
    public fun pluginStatistic() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-statistic:$VERSION")
    public fun pluginUseparameters(): ExternalModuleDependency = dependencyFactory.create("$GROUP:plugin-useparameters:$VERSION")
    public fun pluginExitProcess() : ExternalModuleDependency= dependencyFactory.create("$GROUP:plugin-exitprocess:$VERSION")
}
