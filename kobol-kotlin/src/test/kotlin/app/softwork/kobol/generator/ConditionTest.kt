package app.softwork.kobol.generator

import kotlin.test.*

class ConditionTest {
    @Test
    fun testingIf() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLOIF.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456  IF WORLD = "WORLD!"
        123456* Some Comment
        123456     DISPLAY "HELLO " WORLD
        123456 ELSE
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD
        123456 END-IF.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package helloif
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"
        
        public fun main(): Unit {
          if (WORLD == "WORLD!") {
            // Some Comment
            println("HELLO ${'$'}WORLD")
          } else {
            WORLD = "42"
            println("ANSWER${'$'}WORLD")
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
