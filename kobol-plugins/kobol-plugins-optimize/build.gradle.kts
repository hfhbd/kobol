plugins {
    setup
    repos
}

dependencies {
    api(projects.kobolIr)
    api(projects.kobolPlugins.kobolPluginsPrivate)
    api(projects.kobolPlugins.kobolPluginsReadOnlyVariables)
    api(projects.kobolPlugins.kobolPluginsConstVariables)
    api(projects.kobolPlugins.kobolPluginsCamelCase)
    api(projects.kobolPlugins.kobolPluginsBooleanExpressions)
}
