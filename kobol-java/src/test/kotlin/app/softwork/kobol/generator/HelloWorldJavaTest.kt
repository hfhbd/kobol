package app.softwork.kobol.generator

import app.softwork.kobol.*
import java.io.*
import kotlin.test.*

class HelloWorldJavaTest {
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

        val output = generate(input).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = "WORLD!";
        
          public static void main(String[] args) {
            // Some Comment
            System.out.println("HELLO " + WORLD);
            WORLD = "42";
            System.out.println("ANSWER" + WORLD);
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR() =
    File.createTempFile("testing", ".cbl").apply { writeText(this@toIR) }.toIR()
