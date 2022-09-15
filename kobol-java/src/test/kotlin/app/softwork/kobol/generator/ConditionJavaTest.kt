package app.softwork.kobol.generator

import kotlin.test.*

class ConditionJavaTest {
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
        123456 PROCEDURE                   DIVISION.
        123456  IF WORLD = "WORLD!"
        123456* Some Comment
        123456     DISPLAY "HELLO " WORLD
        123456 ELSE
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD
        123456 END-IF.
        """.trimIndent().toIR()

        val output = generate(input).single()

        //language=java
        val expected = """
        package helloif;
        
        public class Helloif {
          public static String WORLD = "WORLD!";
        
          public static void main(String[] args) {
            if (WORLD.equals("WORLD!")) {
              // Some Comment
              System.out.println("HELLO " + WORLD);
            } else {
              WORLD = "42";
              System.out.println("ANSWER" + WORLD);
            }
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

        val output = generate(input).single()

        //language=java
        val expected = """
        package eval;
        
        public class Eval {
          public static String FOO = "WORLD!";
        
          public static String BAR = "WORLD!";
        
          public static int WORLD = 1;
        
          public static void main(String[] args) {
            if (FOO.equals("") && WORLD == 1 && BAR.equals("")) {
              System.out.println("right");
              System.out.println("two");
            } else {
              System.out.println("else");
              System.out.println("else2");
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

        val output = generate(input).single()

        //language=java
        val expected = """
        package eval;
        
        public class Eval {
          public static String FOO = "WORLD!";
        
          public static int WORLD = 1;
        
          public static void main(String[] args) {
            if (FOO.equals("")) {
              System.out.println("right");
              System.out.println("right2");
            } else {
              System.out.println("else");
              System.out.println("else2");
            }
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
