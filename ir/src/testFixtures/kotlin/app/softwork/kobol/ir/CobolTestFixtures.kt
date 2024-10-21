package app.softwork.kobol.ir

import app.softwork.kobol.toIR
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import kotlin.io.path.toPath
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

object CobolTestFixtures : ReadOnlyProperty<Nothing?, Path> {
    override operator fun getValue(thisRef: Nothing?, property: KProperty<*>): Path {
        val uri = CobolTestFixtures::class.java.getResource("/${property.name}.cbl")!!.toURI()
        val env = mapOf("create" to "true")
        FileSystems.newFileSystem(uri, env)
        val p = uri.toPath()
        return p
    }
}

fun Path.toIR(): KobolIRTree {
    return listOf(this).toIR().single().copy(id = "testing.cbl")
}
