package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class ForEachTest {
    @Test
    fun doWhile() {
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
        123456*COMMENT I
        123456 PERFORM FOO WITH TEST AFTER UNTIL WORLD = 'FOO'.
        123456 FOO SECTION.
        123456   DISPLAY 'FOO'.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        
        public fun FOO() {
          println("FOO")
        }
        
        public var WORLD: String = "WORLD!"
        
        public fun hello() {
          // COMMENT I
          do {
            FOO()
          } while (!(WORLD == "FOO"))
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun performWhile() {
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
        123456*COMMENT I
        123456 PERFORM FOO WITH TEST BEFORE UNTIL WORLD = 'FOO'.
        123456 FOO SECTION.
        123456   DISPLAY 'FOO'.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        
        public fun FOO() {
          println("FOO")
        }
        
        public var WORLD: String = "WORLD!"
        
        public fun hello() {
          // COMMENT I
          while (!(WORLD == "FOO")) {
            FOO()
          }
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun `while`() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456*COMMENT I
        123456 PERFORM UNTIL WORLD = 'FOO'
        123456    DISPLAY 'FOO' 
        123456 END-PERFORM.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        
        public var WORLD: String = "WORLD!"
        
        public fun hello() {
          // COMMENT I
          while (!(WORLD == "FOO")) {
            println("FOO")
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun forEach() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC 9(6) VALUE ZERO.
        123456 PROCEDURE                   DIVISION.
        123456 PERFORM VARYING WORLD FROM 1 BY 3 UNTIL WORLD = 42 
        123456    DISPLAY 'FOO' 
        123456 END-PERFORM.
        123456 PERFORM VARYING WORLD FROM 1 UNTIL WORLD = 42 
        123456    DISPLAY 'FOO' 
        123456 END-PERFORM.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        
        public var WORLD: Int = 0
        
        public fun hello() {
          WORLD = 1
          while (!(WORLD == 42)) {
            println("FOO")
            WORLD += 3
          }
          WORLD = 1
          while (!(WORLD == 42)) {
            println("FOO")
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
