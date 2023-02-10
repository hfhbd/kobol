package app.softwork.kobol.plugins.fir.renaming

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
