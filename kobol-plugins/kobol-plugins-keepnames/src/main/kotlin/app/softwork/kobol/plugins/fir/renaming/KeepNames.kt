package app.softwork.kobol.plugins.fir.renaming

public class KeepNames : Rename(
    functions = {
        keepName()
    },
    variables = {
        keepName()
    },
    classes = {
        keepName()
    }
)

private fun String.keepName() = replace("-", "_")
