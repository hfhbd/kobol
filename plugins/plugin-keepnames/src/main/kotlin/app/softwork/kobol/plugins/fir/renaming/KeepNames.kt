package app.softwork.kobol.plugins.fir.renaming

import app.softwork.kobol.fir.FirPluginBeforePhase
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(FirPluginBeforePhase::class)
public class KeepNames : Rename(
    functions = {
        keepName()
    },
    variables = {
        keepName()
    },
    classes = {
        keepName()
    },
)

private fun String.keepName() = replace("-", "_")
