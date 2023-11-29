package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.plugins.fir.*
import kotlin.test.*

class RecordTest {
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
        123456 05 WO-RLD PIC 9(6).
        123456* BAR I
        123456* BAR II
        123456 01 BAR.
        123456 05 WORLD PIC x(6) VALUE "BAR".
        123456 LINKAGE SECTION.
        123456 77 AAAA PIC A(1).
        123456 01 BBBB.
        123456   05 CCCC PIC A(1).
        123456 PROCEDURE                   DIVISION.
        123456  ADD 42 TO WO-RLD OF FOO
        123456* Some Comment
        123456  DISPLAY "HELLO " WO-RLD OF FOO
        123456  DISPLAY "HELLO " WORLD OF BAR.
        123456  DISPLAY "HELLO " CCCC
        123456  DISPLAY "HELLO " AAAA.
        """.trimIndent().toIR(
            firPlugins = listOf(NullableToZero()),
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        import kotlin.String
        
        public object FOO {
          /**
           * WORLD I
           * WORLD II
           */
          public var `WO-RLD`: Int = 0
        }
        
        /**
         * BAR I
         * BAR II
         */
        public object BAR {
          public var WORLD: String = "BAR"
        }
        
        public object BBBB {
          public var CCCC: String = ""
        }
        
        public fun hello(AAAA: String, BBBB: BBBB) {
          FOO.`WO-RLD` += 42
          // Some Comment
          println("HELLO ${'$'}{FOO.`WO-RLD`}")
          println("HELLO ${'$'}{BAR.WORLD}")
          println("HELLO ${'$'}{BBBB.CCCC}")
          println("HELLO ${'$'}AAAA")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
