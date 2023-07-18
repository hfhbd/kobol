package app.softwork.kobol.generator.java

import kotlin.test.*

class VariableJavaTest {
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

        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = null;
        
          public static void hello() {
            WORLD = "WORLD!";
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
