package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class FileTest {

    @Test
    fun simpleFile() {
        //language=cobol
        val input = """
        |000010 IDENTIFICATION DIVISION.
        |000030 PROGRAM-ID.                 TEST.
        |000160 ENVIRONMENT DIVISION.
        |000210 INPUT-OUTPUT SECTION.
        |000220 FILE-CONTROL.
        |000240     SELECT EIN           ASSIGN UT-S-EB1EIN
        |000250                             FILE STATUS W0AX-EB1.
        |000330 DATA DIVISION.
        |000350 FILE SECTION.
        |000380 FD  EIN
        |000390     RECORDING               V
        |000400     LABEL RECORD            STANDARD
        |000410     DATA RECORD             EB1-EIN.
        |000420
        |000430 01  EB1-EIN.
        |000440     05 FILLER               PIC X(1).
        |123455 WORKING-STORAGE SECTION.
        |123456 77 FI PIC X(1).
        |123456 PROCEDURE DIVISION.
        |123456 OPEN INPUT EIN
        |123456 READ EIN
        |123456   AT END
        |123456     DISPLAY "HELLO " FI 
        |123456   NOT AT END
        |123456     MOVE FILLER TO FI
        |123456 END-READ
        |123456 CLOSE EIN.
        """.trimMargin().toIRFileWithKotlinx()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package test
        
        import app.softwork.serialization.flf.FixedLength
        import java.io.BufferedReader
        import java.io.File
        import kotlin.String
        import kotlin.Unit
        import kotlin.text.charset
        import kotlinx.serialization.ExperimentalSerializationApi
        import kotlinx.serialization.Serializable
        
        @ExperimentalSerializationApi
        @Serializable
        public data class `EB1-EIN`(
          @FixedLength(1)
          public val FILLER: String,
        )
        
        public var FI: String? = null
        
        public fun main(): Unit {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1047"))
          for (`EB1-EIN` in EIN.lineSequence().decode(`EB1-EIN`.serializer())) {
            FI = `EB1-EIN`.FILLER
          }
          println("HELLO ${'$'}FI")
          EIN.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
