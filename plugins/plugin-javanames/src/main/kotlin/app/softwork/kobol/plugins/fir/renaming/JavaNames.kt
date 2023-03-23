package app.softwork.kobol.plugins.fir.renaming

import app.softwork.kobol.fir.FirPluginBeforePhase
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(FirPluginBeforePhase::class)
public class JavaNames : Rename(
    functions = {
        toCamelCase()
    },
    variables = {
        toCamelCase()
    },
    classes = {
        toPascalCase()
    }
)
