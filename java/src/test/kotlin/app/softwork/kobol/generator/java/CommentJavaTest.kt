package app.softwork.kobol.generator.java

import kotlin.test.*

class CommentJavaTest {
    @Test
    fun comments() {
        //language=cobol
        val input = """
        123456* TOP LEVEL I
        123456/* TOP LEVEL II
        123456 IDENTIFICATION              DIVISION.
        123456* PROGRAM ID I
        123456* PROGRAM ID II
        123456 PROGRAM-ID.                 HELLO.
        123456* AUTHOR I
        123456* AUTHOR II
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456* INSTALLATION I
        123456* INSTALLATION II
        123456 INSTALLATION. Softwork.app
        123456* DATE I
        123456* DATE II
        123456 DATE-WRITTEN TODAY.
        123456* ENV I
        123456* ENV II
        123456 ENVIRONMENT DIVISION.
        123456* CONF I
        123456* CONF II
        123456 CONFIGURATION SECTION.
        123456* SPECIAL I
        123456* SPECIAL II
        123456 SPECIAL-NAMES.
        123456* COMMA I
        123456* COMMA II
        123456 DECIMAL-POINT IS COMMA.
        123456* DATA I
        123456* DATA II
        123456 DATA                        DIVISION.
        123456* WORKING I
        123456* WORKING II
        123456 WORKING-STORAGE SECTION.
        123456* HELLO I
        123456* HELLO II
        123456 77 HELLO PIC X(5) VALUE 'HELLO'.
        123456* WORLD I
        123456* WORLD II
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456/* PROCEDURE I
        123456* PROCEDURE II
        123456 PROCEDURE                   DIVISION.
        123456* DISPLAY I
        123456* DISPLAY II
        123456     DISPLAY HELLO WORLD.
        123456
        123456* FOO I
        123456* FOO II
        123456 FOO SECTION.
        123456* MOVE I
        123456* MOVE II
        123456     MOVE HELLO TO WORLD
        123456* DISPLAY ANSWER I
        123456* DISPLAY ANSWER II
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent().toIR()
        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          /**
           * HELLO I
           * HELLO II
           */
          public static String HELLO = "HELLO";
        
          /**
           * WORLD I
           * WORLD II
           */
          public static String WORLD = "WORLD!";
        
          public static void main(String[] args) {
            // DISPLAY I
            // DISPLAY II
            System.out.println(HELLO + WORLD);
            FOO();
          }
        
          /**
           * FOO I
           * FOO II
           */
          public static void FOO() {
            // MOVE I
            // MOVE II
            WORLD = HELLO;
            // DISPLAY ANSWER I
            // DISPLAY ANSWER II
            System.out.println("ANSWER" + WORLD);
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
