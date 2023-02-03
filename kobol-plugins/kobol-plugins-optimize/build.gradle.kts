plugins {
    setup
}

dependencies {
    api(projects.kobolIr)
    api(projects.kobolPlugins.kobolPluginsPrivate)
    api(projects.kobolPlugins.kobolPluginsReadonlyvariables)
    api(projects.kobolPlugins.kobolPluginsConstvariables)
    api(projects.kobolPlugins.kobolPluginsBooleanexpressions)
}
