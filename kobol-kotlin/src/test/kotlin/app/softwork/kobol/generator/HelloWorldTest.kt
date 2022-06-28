package app.softwork.kobol.generator

import app.softwork.kobol.*
import java.io.*
import kotlin.test.*

class HelloWorldTest {
    @Test
    fun helloWorld() {
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
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO"WORLD
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
          println("HELLO${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR() =
    File.createTempFile("testing", ".cobol").apply { writeText(this@toIR) }.toIR()
