package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.ir.optimizations.*
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
        123456 77 WO-RLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO " WO-RLD
        123456     MOVE "42" TO WO-RLD
        123456     DISPLAY WO-RLD
        123456     DISPLAY "ANSWER"WO-RLD.
        """.trimIndent().toIR { _, _ ->
            "main"
        }

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        
        public var `WO-RLD`: String = "WORLD!"
        
        public fun main() {
          // Some Comment
          println("HELLO ${'$'}`WO-RLD`")
          `WO-RLD` = "42"
          println(`WO-RLD`)
          println("ANSWER${'$'}`WO-RLD`")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR(
    vararg including: Pair<String, String>,
    firPlugins: List<FirPlugin> = emptyList(),
    irPlugins: List<IrPlugin> = listOf(NoSynthetics()),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String, File) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
    procedureName: ProcedureName? = null,
): KobolIRTree {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, name).apply { writeText(content) }
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
        },
        controlFlowHandling = controlFlowHandling,
        procedureName = procedureName
    ).single()
}
