package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class RecordTest {
    @Test
    fun simpleRecord() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 01 FOO.
        123456* WORLD I
        123456* WORLD II
        123456 05 WORLD PIC 9(6).
        123456* BAR I
        123456* BAR II
        123456 01 BAR.
        123456 05 WORLD PIC X(6) VALUE "BAR".
        123456 PROCEDURE                   DIVISION.
        123456  MOVE 42 TO WORLD OF FOO
        123456  * Some Comment
        123456  DISPLAY "HELLO " WORLD OF FOO
        123456  DISPLAY "HELLO " WORLD OF BAR.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        import kotlin.String
        import kotlin.Unit
        
        public object FOO {
          /**
           * WORLD I
           * WORLD II
           */
          public var WORLD: Int? = null
        }
        
        /**
         * BAR I
         * BAR II
         */
        public object BAR {
          public var WORLD: String = "BAR"
        }
        
        public fun main(): Unit {
          FOO.WORLD = 42
          // Some Comment
          println("HELLO ${'$'}{FOO.WORLD}")
          println("HELLO ${'$'}{BAR.WORLD}")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
