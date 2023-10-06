package app.softwork.kobol

public fun interface FileHandlingFactory {
    public operator fun invoke(
        packageName: String,
        args: Map<String, String>,
    ): FileHandling
}
