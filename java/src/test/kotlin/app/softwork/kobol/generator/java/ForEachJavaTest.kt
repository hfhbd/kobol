package app.softwork.kobol.generator.java

import kotlin.test.*

class ForEachJavaTest {
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

        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = "WORLD!";
        
          public static void main(String[] args) {
            // COMMENT I
            do {
              FOO();
            } while (!(WORLD.equals("FOO")));
            FOO();
          }
        
          public static void FOO() {
            System.out.println("FOO");
          }
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

        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = "WORLD!";
        
          public static void main(String[] args) {
            // COMMENT I
            while (!(WORLD.equals("FOO"))) {
              System.out.println("FOO");
            }
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
        123456 77 WORLD PIC 9(6) VALUE 1.
        123456 PROCEDURE                   DIVISION.
        123456 PERFORM VARYING WORLD FROM 1 BY 3 UNTIL WORLD = 42 
        123456    DISPLAY 'FOO' 
        123456 END-PERFORM.
        123456 PERFORM VARYING WORLD FROM 1 UNTIL WORLD = 42 
        123456    DISPLAY 'FOO' 
        123456 END-PERFORM.
        """.trimIndent().toIR()

        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static int WORLD = 1;
        
          public static void main(String[] args) {
            for (WORLD = 1; !(WORLD == 42); WORLD += 3) {
              System.out.println("FOO");
            }
            for (WORLD = 1; !(WORLD == 42); WORLD++) {
              System.out.println("FOO");
            }
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
