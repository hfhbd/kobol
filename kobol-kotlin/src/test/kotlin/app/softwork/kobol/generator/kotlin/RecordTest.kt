package app.softwork.kobol.generator.kotlin

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
        123456 05 WORLD PIC 9(6).
        123456* BAR I
        123456* BAR II
        123456 01 BAR.
        123456 05 WORLD PIC x(6) VALUE "BAR".
        123456 PROCEDURE                   DIVISION.
        123456  MOVE 42 TO WORLD OF FOO
        123456  * Some Comment
        123456  DISPLAY "HELLO " WORLD OF FOO
        123456  DISPLAY "HELLO " WORLD OF BAR.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        import kotlin.String
        import kotlin.Unit
        
        public object FOO {
          /**
           * WORLD I
           * WORLD II
           */
          public var WORLD: Int? = null
        }
        
        /**
         * BAR I
         * BAR II
         */
        public object BAR {
          public var WORLD: String = "BAR"
        }
        
        public fun main(): Unit {
          FOO.WORLD = 42
          // Some Comment
          println("HELLO ${'$'}{FOO.WORLD}")
          println("HELLO ${'$'}{BAR.WORLD}")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun emptyRecord() {
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
        123456 05 WORLD.
        123456 10 AA PIC X(3).
        123456 05 BB PIC X(3).
        123456
        123456 77 CC PIC X(6) VALUE "ABCDEF".
        123456 PROCEDURE                   DIVISION.
        123456  MOVE CC TO WORLD OF FOO
        123456  * Some Comment
        123456  DISPLAY "HELLO " AA OF FOO
        123456  DISPLAY "HELLO " BB OF BAR.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        |package hello
        |
        |public object FOO {
        |    /**
        |     * WORLD I
        |     * WORLD II
        |     */
        |    public var WORLD: String?
        |        get() {
        |            return "${'$'}AA${'$'}BB"
        |        } set(value) {
        |        if (value != null) {
        |            require(value.length == 6)
        |        }
        |        AA = value?.substring(0..2)
        |        AA = value?.substring(3..5)
        |    }
        |
        |    public var AA: String? = null
        |
        |    public var BB: String? = null
        |}
        |
        |public var CC: String = "ABCDEF"
        |
        |public fun main(): Unit {
        |    FOO.WORLD = CC
        |    // Some Comment
        |    println("HELLO ${'$'}{FOO.AA}")
        |    println("HELLO ${'$'}{FOO.BB}")
        |}
        |
""".trimMargin()
        assertEquals(expected, output.toString())
    }
}
