plugins {
    setup
    repos
}

dependencies {
    api(projects.kobolIr)
    api(projects.kobolPlugins.kobolPluginsPrivate)
    api(projects.kobolPlugins.kobolPluginsReadonlyvariables)
    api(projects.kobolPlugins.kobolPluginsConstvariables)
    api(projects.kobolPlugins.kobolPluginsCamelcase)
    api(projects.kobolPlugins.kobolPluginsBooleanexpressions)
}