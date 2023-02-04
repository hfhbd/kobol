package app.softwork.kobol.generator.kotlin.optimization

import app.softwork.kobol.generator.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.generator.kotlin.toIR
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.fir.renaming.*
import app.softwork.kobol.plugins.ir.optimizations.*
import org.intellij.lang.annotations.*
import kotlin.test.*

class InliningTest {
    @Test
    fun inlining() {
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
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .let { Inlining()(it, emptyList()) }.single()

        val output = generate(input)

        @Language("kotlin")
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun main(): Unit {
          var HELLO: String = "HELLO"
          var WORLD: String = "WORLD!"
          println("${'$'}HELLO${'$'}WORLD")
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
