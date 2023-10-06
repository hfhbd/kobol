package app.softwork.kobol.ir

import app.softwork.kobol.SerializationPluginFactory
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(SerializationPluginFactory::class)
public class Factory : SerializationPluginFactory {
    override fun invoke(packageName: String, args: Map<String, String>): KotlinxSerialization =
        KotlinxSerialization(packageName)
}
