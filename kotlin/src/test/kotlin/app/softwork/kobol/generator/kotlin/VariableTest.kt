package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.plugins.fir.*
import kotlin.test.*

class VariableTest {
    @Test
    fun noValueAtStart() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6).
        123456 PROCEDURE                   DIVISION.
        123456  MOVE "WORLD!" TO WORLD
        123456  * Some Comment
        123456  DISPLAY "HELLO " WORLD
        123456  MOVE "42" TO WORLD
        123456  DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String? = null
        
        public fun main(): Unit {
          WORLD = "WORLD!"
          // Some Comment
          println("HELLO ${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun math() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC 9(6).
        123456 PROCEDURE                   DIVISION.
        123456  ADD 42 TO WORLD
        123456  * Some Comment
        123456  DISPLAY "HELLO " WORLD
        123456  SUBTRACT 42 FROM WORLD
        123456  DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR(firPlugins = listOf(NullableToZero()))

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        import kotlin.Unit
        
        public var WORLD: Int = 0
        
        public fun main(): Unit {
          WORLD += 42
          // Some Comment
          println("HELLO ${'$'}WORLD")
          WORLD -= 42
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
