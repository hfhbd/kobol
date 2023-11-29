package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class CallTest {
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

        val output = generate(input)

        //language=kotlin
        val expected = """
        package calling
        
        public fun FOO() {
          LONDON()
        }
        
        public object LONDON {
          init {
            System.loadLibrary("london")
          }
        
          public external operator fun invoke()
        }
        
        public fun calling() {
          // LONDON CALLING
          LONDON()
          println("FOO")
          FOO()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun linkageTest() {
        //language=cobol
        val input = """
        |       IDENTIFICATION DIVISION.
        |       PROGRAM-ID. CALLING.
        |       DATA DIVISION.
        |       WORKING-STORAGE SECTION.
        |       01 FOO.
        |       05 BAR PIC 9 VALUE 5.
        |       05 C PIC X VALUE 'X'.
        |       PROCEDURE DIVISION.
        |      * LONDON CALLING
        |           CALL "LONDON" USING FOO.
        |           DISPLAY "FOO".
        |       FOO SECTION.
        |           CALL "LONDON" USING FOO.
        """.trimMargin().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        |package calling
        |
        |import kotlin.Int
        |import kotlin.String
        |
        |public fun FOO() {
        |  LONDON(FOO.BAR, FOO.C)
        |}
        |
        |public object FOO {
        |  public var BAR: Int = 5
        |
        |  public var C: String = "X"
        |}
        |
        |public object LONDON {
        |  init {
        |    System.loadLibrary("london")
        |  }
        |
        |  public external operator fun invoke(BAR: Int, C: String)
        |}
        |
        |public fun calling() {
        |  // LONDON CALLING
        |  LONDON(FOO.BAR, FOO.C)
        |  println("FOO")
        |  FOO()
        |}
        |
        """.trimMargin()
        assertEquals(expected, output.toString())
    }
}
