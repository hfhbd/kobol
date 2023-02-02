package app.softwork.kobol.plugins.ir.optimizations

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
