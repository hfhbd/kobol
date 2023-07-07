plugins {
    id("kotlinSetup")
}

dependencies {
    api(projects.ir)
    api(projects.plugins.pluginPrivate)
    api(projects.plugins.pluginReadonlyvariables)
    api(projects.plugins.pluginConstvariables)
    api(projects.plugins.pluginBooleanexpressions)
}
