package app.softwork.kobol.generator.java

import app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory.*
import kotlin.test.*

class SectionJavaTest {
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
        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = "WORLD!";
        
          public static String HELLO = "HELLO";
        
          public static void main(String[] args) {
            System.out.println(HELLO + WORLD);
            FOO();
          }
        
          public static void FOO() {
            // Some Comment
            WORLD = "42";
            System.out.println("ANSWER" + WORLD);
          }
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
        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package hello;
        
        public class Hello {
          public static String WORLD = "WORLD!";
        
          public static String HELLO = "HELLO";
        
          public static void main(String[] args) {
            FOO();
          }
        
          public static void FOO() {
            // Some Comment
            WORLD = "42";
            System.out.println("ANSWER" + WORLD);
          }
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
            STOP RUN.

            BAR SECTION.
            DISPLAY "BAR"
            STOP RUN.

            C SECTION.
            DISPLAY "C".
        """.trimIndent().toIR(controlFlowHandling = {
            ExitProcessControlFlowHandling
        })

        val output = generate(input, java8 = true).single()

        //language=java
        val expected = """
        package calling;
        
        public class Calling {
          public static void main(String[] args) {
            FOO();
            BAR();
            C();
          }
        
          public static void FOO() {
            System.out.println("FOO");
            BAR();
            System.out.println("FOO2");
            System.exit(0);
          }
        
          public static void BAR() {
            System.out.println("BAR");
            System.exit(0);
          }
        
          public static void C() {
            System.out.println("C");
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
