package app.softwork.kobol.ir

public class Factory : SerializationPluginFactory {
    override fun invoke(packageName: String, args: Map<String, String>): KotlinxSerialization =
        KotlinxSerialization(packageName)
}
