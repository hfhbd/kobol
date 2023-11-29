package app.softwork.kobol.generator.java

import kotlin.test.*

class RecordJavaTest {
    @Test
    fun simpleRecord() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 01 FOO.
        123456* WORLD I
        123456* WORLD II
        123456 05 WORLD PIC 9(6).
        123456* BAR I
        123456* BAR II
        123456 01 BAR.
        123456 05 WORLD PIC X(6) VALUE "BAR".
        123456 PROCEDURE                   DIVISION.
        123456  MOVE 42 TO WORLD OF FOO
        123456* Some Comment
        123456  DISPLAY "HELLO " WORLD OF FOO
        123456  DISPLAY "HELLO " WORLD OF BAR.
        """.trimIndent().toIR()

        val (foo, bar, output) = generate(input, java8 = true)

        assertEquals( //language=java
            """
        package hello;
        
        public class FOO {
          /**
           * WORLD I
           * WORLD II
           */
          public static Integer WORLD = null;
        }
        
            """.trimIndent(),
            foo.toString(),
        )

        assertEquals( //language=java
            """
        package hello;
        
        /**
         * BAR I
         * BAR II
         */
        public class BAR {
          public static String WORLD = "BAR";
        }
        
            """.trimIndent(),
            bar.toString(),
        )

        assertEquals( //language=java
            """
        package hello;
        
        public class Hello {
          public static void hello() {
            FOO.WORLD = 42;
            // Some Comment
            System.out.println("HELLO " + FOO.WORLD);
            System.out.println("HELLO " + BAR.WORLD);
          }
        }
        
            """.trimIndent(),
            output.toString(),
        )
    }
}
