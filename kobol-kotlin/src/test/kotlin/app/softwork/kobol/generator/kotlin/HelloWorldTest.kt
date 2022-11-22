package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.*
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

internal fun String.toIR(vararg including: Pair<String, String>): KobolIRTree {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, "$name.cbl").apply { writeText(content) }
    }
    return (files + File(temp, "testing.cbl").apply { writeText(this@toIR) }).toIR().single()
}

internal fun String.toIRFileWithKotlinx(firPlugins: List<FirPlugin> = emptyList(), vararg including: Pair<String, String>): KobolIRTree {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, "$name.cbl").apply { writeText(content) }
    }
    return (files + File(temp, "testing.cbl").apply { writeText(this@toIRFileWithKotlinx) }).toIR(
        firPlugins = firPlugins,
        fileConverter = {
            JavaFilesKotlin()
        },
        serialization = {
            KotlinxSerialization(it)
        }
    ).single()
}

internal fun String.toIRWithSql(firPlugins: List<FirPlugin> = emptyList(), vararg including: Pair<String, String>): Pair<KobolIRTree, SqFiles> {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, "$name.cbl").apply { writeText(content) }
    }
    var sqlCompiler: SqlDelightPrecompiler? = null
    val kobol = (files + File(temp, "testing.cbl").apply { writeText(this@toIRWithSql) }).toIR(
        firPlugins = firPlugins,
        sqlPrecompiler = {
            sqlCompiler = SqlDelightPrecompiler(dbName = "DB", temp, it, it)
            sqlCompiler!!
        }
    ).single()

    return kobol to sqlCompiler!!.files!!
}
