package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.sqldelightprecompiler.*
import app.softwork.sqldelightwriter.*
import java.io.*
import java.nio.file.*
import kotlin.test.*

class HelloWorldTest {
    @Test
    fun helloWorld() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO " WORLD
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"
        
        public fun main(): Unit {
          // Some Comment
          println("HELLO ${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR(
    firPlugins: List<FirPlugin> = emptyList(),
    vararg including: Pair<String, String>,
    irPlugins: List<IrPlugin> = emptyList(),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String, File) -> SqlPrecompiler)? = null,
): KobolIRTree {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, "$name.cbl").apply { writeText(content) }
    }
    return (files + File(temp, "testing.cbl").apply { writeText(this@toIR) }).toIR(
        firPlugins = firPlugins,
        irPlugins = irPlugins,
        fileConverter = fileConverter,
        serialization = serialization,
        sqlPrecompiler = sqlPrecompiler?.let { getSQL ->
            {
                getSQL(it, temp)
            }
        }
    ).single()
}
