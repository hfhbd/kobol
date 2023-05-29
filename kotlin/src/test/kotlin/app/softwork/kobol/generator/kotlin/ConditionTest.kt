package app.softwork.kobol.generator.kotlin

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
        123456 77 FOO PIC 9(2) VALUE 10.
        123456 PROCEDURE                   DIVISION.
        123456  IF WORLD = "WORLD!"
        123456* Some Comment
        123456     DISPLAY "HELLO " WORLD
        123456 ELSE
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD
        123456 END-IF
        123546 IF FOO LESS THAN 42
        123456     DISPLAY "FOO"
        123456 END-IF
        123546 IF FOO LESS THAN OR EQUAL TO 42
        123456     DISPLAY "FOO"
        123456 END-IF.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package helloif
        
        import kotlin.Int
        import kotlin.String
        
        public var WORLD: String = "WORLD!"
        
        public var FOO: Int = 10
        
        public fun main() {
          if (WORLD == "WORLD!") {
            // Some Comment
            println("HELLO ${'$'}WORLD")
          } else {
            WORLD = "42"
            println("ANSWER${'$'}WORLD")
          }
          if (FOO < 42) {
            println("FOO")
          }
          if (FOO <= 42) {
            println("FOO")
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun eval() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 EVAL.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 FOO PIC X(6) VALUE 'WORLD!'.
        123456 77 BAR PIC X(6) VALUE 'WORLD!'.
        123456 77 WORLD PIC 9(6) VALUE 1.
        123456 PROCEDURE                   DIVISION.
        123456 EVALUATE FOO ALSO WORLD ALSO BAR
        123456 WHEN "" ALSO 1 ALSO ""
        123456 DISPLAY "right"
        123456 DISPLAY "two"
        123456 WHEN OTHER
        123456 DISPLAY "else"
        123456 DISPLAY "else2"
        123456 END-EVALUATE.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package eval
        
        import kotlin.Int
        import kotlin.String
        
        public var FOO: String = "WORLD!"
        
        public var BAR: String = "WORLD!"
        
        public var WORLD: Int = 1
        
        public fun main() {
          when {
            FOO == "" && WORLD == 1 && BAR == "" -> {
              println("right")
              println("two")
            }
            else -> {
              println("else")
              println("else2")
            }
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun evalSingle() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 EVAL.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 FOO PIC X(6) VALUE 'WORLD!'.
        123456 77 WORLD PIC 9(6) VALUE 1.
        123456 PROCEDURE                   DIVISION.
        123456 EVALUATE FOO
        123456 WHEN ""
        123456 DISPLAY "right"
        123456 DISPLAY "right2"
        123456 WHEN OTHER
        123456 DISPLAY "else"
        123456 DISPLAY "else2"
        123456 END-EVALUATE.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package eval
        
        import kotlin.Int
        import kotlin.String
        
        public var FOO: String = "WORLD!"
        
        public var WORLD: Int = 1
        
        public fun main() {
          when (FOO) {
            "" -> {
              println("right")
              println("right2")
            }
            else -> {
              println("else")
              println("else2")
            }
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
