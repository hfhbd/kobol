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
        """.trimMargin().toIRFileWithKotlinx()

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
        )
        
        @ExperimentalSerializationApi
        @Serializable
        public data class `EB1-AUS`(
          @FixedLength(1)
          public var FILLER: String,
        )
        
        public val `EB1-AUS` = `EB1-AUS`()
        
        public var FI: String? = null
        
        public fun main(): Unit {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1047"))
          val AUS: BufferedWriter = File("AUS").bufferedWriter(charset("IBM-1047"))
          for (`EB1-EIN` in EIN.lineSequence().decode(`EB1-EIN`.serializer())) {
            FI = `EB1-EIN`.FILLER
            `EB1-AUS`.FILLER = `EB1-EIN`.FILLER
            AUS.append(`EB1-AUS`.serializer(), `EB1-AUS`)
          }
          println("HELLO ${'$'}FI")
          EIN.close()
          AUS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    // https://github.com/neopragma/cobol-samples/blob/main/src/main/cobol/CPSEQVR.CBL
    @Test
    fun sample() {
        //language=cobol
        val input = """
      |000000*****************************************************************
      |      * Program name:    CPSEQFR
      |      * Original author: Dave Nicolette
      |      *
      |      * Demonstrates:
      |      
      |      * How to read and write sequential datasets (QSAM) with
      |      * variable-length records.
      |      *
      |      * This program reads VARFILE1, appends data to each record, and
      |      * writes the records to VARFILE2. It also counts the number of
      |      * records processed.
      |      ****************************************************************
      |      IDENTIFICATION DIVISION.
      |      PROGRAM-ID.  CPSEQVR.
      |      ENVIRONMENT DIVISION.
      |      INPUT-OUTPUT SECTION.
      |      FILE-CONTROL.
      |          SELECT INFILE ASSIGN  TO 'VARFILE1'
      |                 FILE STATUS IS INPUT-FILE-STATUS.
      |          SELECT OUTFILE ASSIGN  TO 'VARFILE2'
      |                 FILE STATUS IS OUTPUT-FILE-STATUS.
      |      DATA DIVISION.
      |      FILE SECTION.
      |      FD  INFILE
      |          DATA RECORD IS INPUT-RECORD
      |          RECORDING MODE IS V
      |          BLOCK CONTAINS 0
      |          RECORD IS VARYING 5 TO 50 DEPENDING ON IN-RECLEN.
      |      01  INPUT-RECORD            PIC X(50).
      |      FD  OUTFILE
      |          DATA RECORD IS OUTPUT-RECORD
      |          RECORDING MODE IS V
      |          BLOCK CONTAINS 0
      |          RECORD IS VARYING 5 TO 50 DEPENDING ON OUT-RECLEN.
      |      01  OUTPUT-RECORD            PIC X(50).
      |      WORKING-STORAGE SECTION.
      |      01  WorkAreas.
      |          05  IN-RECLEN            PIC S9(9) COMP.
      |          05  OUT-RECLEN           PIC S9(4) COMP.
      |          05  INPUT-FILE-STATUS    PIC X(02).
      |              88  INFILE-OK        VALUE '00'.
      |              88  END-OF-INPUT     VALUE '10'.
      |          05  OUTPUT-FILE-STATUS   PIC X(02).
      |              88  OUTFILE-OK       VALUE '00'.
      |          05  RECORD-COUNT         PIC S9(5) COMP-3.
      |          05  FIELD-COUNT          PIC S9(3) COMP-3.
      |      01  RECORD-AREA.
      |          05  RECORD-FIELD OCCURS 1 TO 10 DEPENDING ON FIELD-COUNT
      |                                   PIC X(05).
      |
      |      PROCEDURE DIVISION.
      |          OPEN INPUT INFILE
      |          IF NOT INFILE-OK
      |              DISPLAY 'INFILE STATUS ON OPEN: ' INPUT-FILE-STATUS
      |              GO TO END-OF-PROGRAM
      |          END-IF
      |          OPEN OUTPUT OUTFILE
      |          IF NOT OUTFILE-OK
      |              DISPLAY 'OUTFILE STATUS ON OPEN: ' OUTPUT-FILE-STATUS
      |              GO TO END-OF-PROGRAM
      |          END-IF
      |          PERFORM UNTIL END-OF-INPUT
      |              READ INFILE
      |              IF INFILE-OK
      |                  COMPUTE FIELD-COUNT =
      |                      IN-RECLEN / LENGTH OF RECORD-FIELD
      |                  MOVE INPUT-RECORD TO RECORD-AREA
      |                  ADD 1 TO FIELD-COUNT
      |                  MOVE 'XXXXX' TO RECORD-FIELD(FIELD-COUNT)
      |                  COMPUTE OUT-RECLEN =
      |                      FIELD-COUNT * LENGTH OF RECORD-FIELD
      |                  WRITE OUTPUT-RECORD FROM RECORD-AREA
      |                  ADD 1 TO RECORD-COUNT
      |          END-PERFORM
      |              .
      |      END-OF-PROGRAM.
      |          DISPLAY 'NUMBER OF RECORDS PROCESSED: ' RECORD-COUNT
      |          CLOSE INFILE
      |          CLOSE OUTFILE
      |          GOBACK.

        """.trimMargin().toIRFileWithKotlinx()

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
        )
        
        @ExperimentalSerializationApi
        @Serializable
        public data class `EB1-AUS`(
          @FixedLength(1)
          public var FILLER: String,
        )
        
        public val `EB1-AUS` = `EB1-AUS`()
        
        public var FI: String? = null
        
        public fun main(): Unit {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1047"))
          val AUS: BufferedWriter = File("AUS").bufferedWriter(charset("IBM-1047"))
          for (`EB1-EIN` in EIN.lineSequence().decode(`EB1-EIN`.serializer())) {
            FI = `EB1-EIN`.FILLER
            `EB1-AUS`.FILLER = `EB1-EIN`.FILLER
            AUS.append(`EB1-AUS`.serializer(), `EB1-AUS`)
          }
          println("HELLO ${'$'}FI")
          EIN.close()
          AUS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
