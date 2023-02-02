package app.softwork.kobol.plugins.ir.optimizations

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
