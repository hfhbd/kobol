package app.softwork.kobol.generator

import org.intellij.lang.annotations.*
import kotlin.test.*

class SectionTest {
    @Test
    fun helloWorldSection() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY "HELLO"WORLD
        123456     PERFORM FOO.
        123456            
        123456 FOO SECTION.
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
        val output = KotlinGenerator.generate(input)

        @Language("Kotlin") val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun FOO(): Unit {
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        public var WORLD: String = "WORLD!"
        
        public fun main(): Unit {
          println("HELLO${'$'}WORLD")
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
