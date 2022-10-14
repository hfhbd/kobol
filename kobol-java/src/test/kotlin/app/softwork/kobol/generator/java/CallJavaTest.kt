package app.softwork.kobol.generator.java

import kotlin.test.*

class CallJavaTest {
    @Test
    fun londonCalling() {
        //language=cobol
        val input = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. CALLING.
            DATA DIVISION.
            PROCEDURE DIVISION.
           * LONDON CALLING
                CALL "LONDON"
                DISPLAY "FOO".
            FOO SECTION.
                CALL "LONDON".
        """.trimIndent().toIR()

        val (londonJava, output) = generate(input, java8 = true).map { it.toString() }

        //language=java
        val london = """
        package calling;
        
        public class LONDON {
          static {
            System.loadLibrary("london");
          }
        
          public static native void invoke();
        }
        
        """.trimIndent()

        assertEquals(london, londonJava)

        //language=java
        val expected = """
        package calling;
        
        public class Calling {
          public static void main(String[] args) {
            // LONDON CALLING
            LONDON.invoke();
            System.out.println("FOO");
            FOO();
          }
        
          public static void FOO() {
            LONDON.invoke();
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output)
    }

    @Test
    fun linkageTest() {
        //language=cobol
        val input = """
        |      IDENTIFICATION DIVISION.
        |      PROGRAM-ID. CALLING.
        |      DATA DIVISION.
        |      WORKING-STORAGE SECTION.
        |      01 FOO.
        |      05 BAR PIC 9 VALUE 5.
        |      PROCEDURE DIVISION.
        |     * LONDON CALLING
        |          CALL "LONDON" USING FOO.
        |          DISPLAY "FOO".
        |      FOO SECTION.
        |          CALL "LONDON" USING FOO.
        """.trimMargin().toIR()

        val (fooJava, londonJava, output) = generate(input, java8 = true)

        //language=java
        val foo = """
        package calling;
        
        public class FOO {
          public static int BAR = 5;
        }
        
        """.trimIndent()
        assertEquals(foo, fooJava.toString())

        //language=java
        val london = """
        package calling;
        
        public class LONDON {
          static {
            System.loadLibrary("london");
          }
        
          public static native void invoke(int BAR);
        }
        
        """.trimIndent()
        assertEquals(london, londonJava.toString())

        //language=java
        val expected = """
        package calling;
        
        public class Calling {
          public static void main(String[] args) {
            // LONDON CALLING
            LONDON.invoke(FOO.BAR);
            System.out.println("FOO");
            FOO();
          }
        
          public static void FOO() {
            LONDON.invoke(FOO.BAR);
          }
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
