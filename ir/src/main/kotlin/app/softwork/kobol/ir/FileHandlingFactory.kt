package app.softwork.kobol.ir

public fun interface FileHandlingFactory {
    public operator fun invoke(
        packageName: String,
        args: Map<String, String>,
    ): FileHandling
}
