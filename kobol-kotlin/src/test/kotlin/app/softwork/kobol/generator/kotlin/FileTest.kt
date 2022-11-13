package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.*
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
        |000240     SELECT EIN           ASSIGN A
        |000250                             FILE STATUS EIN-STATUS.
        |000240     SELECT AUS           ASSIGN B
        |000250                             FILE STATUS AUS-STATUS.
        |000330 DATA DIVISION.
        |000350 FILE SECTION.
        |000380 FD  EIN
        |000390     RECORDING               V
        |000400     LABEL RECORD            STANDARD
        |000410     DATA RECORD             EB1-EIN.
        |000420
        |000430 01  EB1-EIN.
        |000440     05 FILLER               PIC X(1).
        |000380 FD  AUS
        |000400     LABEL RECORD            STANDARD
        |000390     RECORDING               V
        |000410     DATA RECORD             EB1-AUS.
        |000420
        |000430 01  EB1-AUS.
        |000440     05 FILLER               PIC X(1).
        |123455 WORKING-STORAGE SECTION.
        |123456 77 FI PIC X(1).
        |123456 PROCEDURE DIVISION.
        |123456 OPEN INPUT EIN
        |123456 OPEN OUTPUT AUS
        |123456 READ EIN
        |123456   AT END
        |123456     DISPLAY "HELLO " FI 
        |123456   NOT AT END
        |123456     MOVE FILLER OF EB1-EIN TO FI
        |123456     MOVE FILLER OF EB1-EIN TO FILLER OF EB1-AUS
        |123456     WRITE EB1-AUS
        |123456 END-READ
        |123456 CLOSE EIN.
        |123456 CLOSE AUS.
        """.trimMargin().toIRFileWithKotlinx(listOf(NullableToZero))

        val output = generate(input)

        //language=kotlin
        val expected = """
        package test
        
        import app.softwork.serialization.flf.FixedLength
        import app.softwork.serialization.flf.append
        import java.io.BufferedReader
        import java.io.BufferedWriter
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
        ) {
          public companion object {
            public var FILLER: String = ""
        
            public fun create(): `EB1-EIN` {
              return `EB1-EIN`(FILLER)
            }
          }
        }
        
        @ExperimentalSerializationApi
        @Serializable
        public data class `EB1-AUS`(
          @FixedLength(1)
          public val FILLER: String,
        ) {
          public companion object {
            public var FILLER: String = ""
        
            public fun create(): `EB1-AUS` {
              return `EB1-AUS`(FILLER)
            }
          }
        }
        
        public var FI: String = ""
        
        public fun main(): Unit {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1047"))
          val AUS: BufferedWriter = File("AUS").bufferedWriter(charset("IBM-1047"))
          for (`EB1-EIN` in EIN.lineSequence().decode(`EB1-EIN`.serializer())) {
            FI = `EB1-EIN`.FILLER
            `EB1-AUS`.FILLER = `EB1-EIN`.FILLER
            AUS.append(`EB1-AUS`.serializer(), `EB1-AUS`.create())
          }
          println("HELLO ${'$'}FI")
          EIN.close()
          AUS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
