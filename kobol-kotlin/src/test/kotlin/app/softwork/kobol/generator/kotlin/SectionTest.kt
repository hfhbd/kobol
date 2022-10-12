package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class SectionTest {
    @Test
    fun performSection() {
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
        123456     DISPLAY HELLO WORLD.
        123456            
        123456 FOO SECTION.
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR()
        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun FOO(): Unit {
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        public var WORLD: String = "WORLD!"
        
        public var HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun emptyTopLevel() {
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
        123456            
        123456 FOO SECTION.
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR()
        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun FOO(): Unit {
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        public var WORLD: String = "WORLD!"
        
        public var HELLO: String = "HELLO"
        
        public fun main(): Unit {
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun complexSection() {
        //language=cobol
        val input = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. CALLING.
            DATA DIVISION.
            PROCEDURE DIVISION.

            FOO SECTION.
            DISPLAY "FOO"
            PERFORM BAR
            DISPLAY "FOO2"
            GOBACK.

            BAR SECTION.
            DISPLAY "BAR"
            GOBACK.

            C SECTION.
            DISPLAY "C".
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package calling
        
        import kotlin.Nothing
        import kotlin.Unit
        import kotlin.system.exitProcess
        
        public fun FOO(): Nothing {
          println("FOO")
          BAR()
          println("FOO2")
          return exitProcess(0)
        }
        
        public fun BAR(): Nothing {
          println("BAR")
          return exitProcess(0)
        }
        
        public fun C(): Unit {
          println("C")
        }
        
        public fun main(): Unit {
          FOO()
          BAR()
          C()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
