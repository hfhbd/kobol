package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.fir.*
import app.softwork.kobol.plugins.fir.renaming.KeepNames
import app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory
import app.softwork.kobol.plugins.ir.optimizations.BooleanExpressions
import app.softwork.kobol.plugins.ir.optimizations.Inlining
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
        |000250                          FILE STATUS EIN-STATUS.
        |000240     SELECT AUS           ASSIGN B
        |000250                          FILE STATUS AUS-STATUS.
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
        |000440     05 FOO                  PIC S9(1) SIGN IS LEADING SEPARATE.
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
        |123456 CLOSE EIN AUS.
        """.trimMargin().toIR(
            firPlugins = listOf(NullableToZero()),
            fileConverter = {
                JavaFilesKotlin()
            },
            serialization = {
                KotlinxSerialization(it)
            },
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package test
        
        import app.softwork.serialization.flf.Ebcdic
        import app.softwork.serialization.flf.Ebcdic.Format
        import app.softwork.serialization.flf.FixedLength
        import app.softwork.serialization.flf.FixedLengthFormat
        import app.softwork.serialization.flf.append
        import app.softwork.serialization.flf.decode
        import java.io.BufferedReader
        import java.io.BufferedWriter
        import java.io.File
        import kotlin.Int
        import kotlin.String
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
          @Ebcdic(Format.Zoned)
          @FixedLength(1)
          public val FOO: Int,
        ) {
          public companion object {
            public var FILLER: String = ""
        
            public var FOO: Int = 0
        
            public fun create(): `EB1-AUS` {
              return `EB1-AUS`(FILLER, FOO)
            }
          }
        }
        
        public var FI: String = ""
        
        public fun test() {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1140"))
          val AUS: BufferedWriter = File("AUS").bufferedWriter(charset("IBM-1140"))
          for (`EB1-EIN` in EIN.decode(`EB1-EIN`.serializer(), FixedLengthFormat(""))) {
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

    @Test
    fun lineSequential() {
        //language=cobol
        val input = """
        |000010 IDENTIFICATION DIVISION.
        |000030 PROGRAM-ID.                 TEST.
        |000160 ENVIRONMENT DIVISION.
        |000210 INPUT-OUTPUT SECTION.
        |000220 FILE-CONTROL.
        |000240     SELECT EIN           ASSIGN A
        |111111                          ORGANIZATION IS LINE SEQUENTIAL
        |000250                          FILE STATUS EIN-STATUS.
        |000240     SELECT AUS           ASSIGN B
        |111111                          ORGANIZATION IS LINE SEQUENTIAL
        |000250                          FILE STATUS AUS-STATUS.
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
        """.trimMargin().toIR(
            firPlugins = listOf(NullableToZero()),
            fileConverter = {
                JavaFilesKotlin()
            },
            serialization = {
                KotlinxSerialization(it)
            },
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package test
        
        import app.softwork.serialization.flf.FixedLength
        import app.softwork.serialization.flf.appendLine
        import app.softwork.serialization.flf.decode
        import java.io.BufferedReader
        import java.io.BufferedWriter
        import java.io.File
        import kotlin.String
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
        
        public fun test() {
          val EIN: BufferedReader = File("EIN").bufferedReader(charset("IBM-1140"))
          val AUS: BufferedWriter = File("AUS").bufferedWriter(charset("IBM-1140"))
          for (`EB1-EIN` in EIN.lineSequence().decode(`EB1-EIN`.serializer())) {
            FI = `EB1-EIN`.FILLER
            `EB1-AUS`.FILLER = `EB1-EIN`.FILLER
            AUS.appendLine(`EB1-AUS`.serializer(), `EB1-AUS`.create())
          }
          println("HELLO ${'$'}FI")
          EIN.close()
          AUS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun transactions() {
        //language=cobol
        val input = """
        |000010 IDENTIFICATION DIVISION.
        |000020 PROGRAM-ID.                 FILES.
        |000030 ENVIRONMENT DIVISION.
        |000040 INPUT-OUTPUT SECTION.
        |000050 FILE-CONTROL.
        |000060     SELECT TRANSACTIONS     ASSIGN T.
        |000080     SELECT BALANCES         ASSIGN B
        |000090                             FILE STATUS B-STATUS.
        |000100 DATA DIVISION.
        |000110 FILE SECTION.
        |000120 FD  TRANSACTIONS.
        |000170 01  TRANSACTION.
        |000180     02 FIRSTNAME            PIC X(10).
        |000190     02 LASTNAME             PIC X(10).
        |000190     02 TRANSACTION          PIC S9(6).
        |000191
        |000200 FD  BALANCES
        |000210     LABEL RECORD            STANDARD
        |000220     RECORDING               V
        |000230     DATA RECORD             EB1-AUS.
        |000250 01  BALANCE.
        |000260     02 FIRSTNAME               PIC X(10).
        |000260     02 LASTNAME                PIC X(10).
        |000260     02 BALANCE                 PIC S9(6).
        |000270 WORKING-STORAGE SECTION.
        |000000 77 T-STATUS PIC X(2).
        |000290 77 B-STATUS PIC X(2).
        |000300 77 COUNT PIC 9(4).
        |123456
        |123456 PROCEDURE DIVISION.
        |123456     OPEN INPUT TRANSACTIONS
        |123456     OPEN OUTPUT BALANCES
        |123456     READ TRANSACTIONS
        |123456       AT END
        |123456         DISPLAY "COUNT " COUNT
        |123456       NOT AT END
        |123456         ADD ZEROS TO COUNT
        |111111         IF FIRSTNAME OF TRANSACTION = FIRSTNAME OF BALANCE AND
        |000000            LASTNAME OF TRANSACTION = LASTNAME OF BALANCE THEN
        |000000              ADD TRANSACTION OF TRANSACTION TO BALANCE OF BALANCE
        |000000         ELSE
        |111111            WRITE BALANCE
        |111111            MOVE FIRSTNAME OF TRANSACTION TO FIRSTNAME OF BALANCE
        |111111            MOVE LASTNAME OF TRANSACTION TO LASTNAME OF BALANCE
        |111111            MOVE TRANSACTION OF TRANSACTION TO BALANCE OF BALANCE
        |000000         END-IF
        |123456         WRITE BALANCE
        |123456     END-READ
        |123456     CLOSE BALANCES.
        |123456     CLOSE TRANSACTIONS.
        |
        """.trimMargin().toIR(
            firPlugins = listOf(NullableToZero()),
            fileConverter = {
                JavaFilesKotlin()
            },
            serialization = {
                KotlinxSerialization(it)
            },
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package files
        
        import app.softwork.serialization.flf.Ebcdic
        import app.softwork.serialization.flf.Ebcdic.Format
        import app.softwork.serialization.flf.FixedLength
        import app.softwork.serialization.flf.FixedLengthFormat
        import app.softwork.serialization.flf.append
        import app.softwork.serialization.flf.decode
        import java.io.BufferedReader
        import java.io.BufferedWriter
        import java.io.File
        import kotlin.Int
        import kotlin.String
        import kotlin.text.charset
        import kotlinx.serialization.ExperimentalSerializationApi
        import kotlinx.serialization.Serializable
        
        @ExperimentalSerializationApi
        @Serializable
        public data class TRANSACTION(
          @FixedLength(10)
          public val FIRSTNAME: String,
          @FixedLength(10)
          public val LASTNAME: String,
          @Ebcdic(Format.Zoned)
          @FixedLength(6)
          public val TRANSACTION: Int,
        ) {
          public companion object {
            public var FIRSTNAME: String = ""
        
            public var LASTNAME: String = ""
        
            public var TRANSACTION: Int = 0
        
            public fun create(): TRANSACTION {
              return TRANSACTION(FIRSTNAME, LASTNAME, TRANSACTION)
            }
          }
        }
        
        @ExperimentalSerializationApi
        @Serializable
        public data class BALANCE(
          @FixedLength(10)
          public val FIRSTNAME: String,
          @FixedLength(10)
          public val LASTNAME: String,
          @Ebcdic(Format.Zoned)
          @FixedLength(6)
          public val BALANCE: Int,
        ) {
          public companion object {
            public var FIRSTNAME: String = ""
        
            public var LASTNAME: String = ""
        
            public var BALANCE: Int = 0
        
            public fun create(): BALANCE {
              return BALANCE(FIRSTNAME, LASTNAME, BALANCE)
            }
          }
        }
        
        public var `T-STATUS`: String = ""
        
        public var `B-STATUS`: String = ""
        
        public var COUNT: Int = 0
        
        public fun files() {
          val TRANSACTIONS: BufferedReader = File("TRANSACTIONS").bufferedReader(charset("IBM-1140"))
          val BALANCES: BufferedWriter = File("BALANCES").bufferedWriter(charset("IBM-1140"))
          for (TRANSACTION in TRANSACTIONS.decode(TRANSACTION.serializer(), FixedLengthFormat(""))) {
            COUNT += 0
            if (TRANSACTION.FIRSTNAME == BALANCE.FIRSTNAME && TRANSACTION.LASTNAME == BALANCE.LASTNAME) {
              BALANCE.BALANCE += TRANSACTION.TRANSACTION
            } else {
              BALANCES.append(BALANCE.serializer(), BALANCE.create())
              BALANCE.FIRSTNAME = TRANSACTION.FIRSTNAME
              BALANCE.LASTNAME = TRANSACTION.LASTNAME
              BALANCE.BALANCE = TRANSACTION.TRANSACTION
            }
            BALANCES.append(BALANCE.serializer(), BALANCE.create())
          }
          println("COUNT ${'$'}COUNT")
          BALANCES.close()
          TRANSACTIONS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun files() {
        //language=cobol
        val input = """
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.                 FILES.
000030 ENVIRONMENT DIVISION.
000040 INPUT-OUTPUT SECTION.
000050 FILE-CONTROL.
000060     SELECT TRANSACTS        ASSIGN TO "TRANSACT".
000080     SELECT BALANCES         ASSIGN TO "BALANCES".
000100 DATA DIVISION.
000110 FILE SECTION.
000120 FD  TRANSACTS
000130     RECORDING               V
000150     DATA RECORD             EB1-EIN.
000170 01  TRANSACTION.
000180     02 FIRSTNAME            PIC X(10).
000190     02 LASTNAME             PIC X(10).
000190     02 TRANSACTION          PIC S9(7).
000191
000200 FD  BALANCES
000220     RECORDING               V
000230     DATA RECORD             EB1-AUS.
000250 01  BALANCE.
000260     02 FIRSTNAME             PIC X(10).
000260     02 LASTNAME              PIC X(10).
000260     02 SALDO                 PIC S9(11).
000270 WORKING-STORAGE SECTION.
000300 77 COUNTER  PIC 9(4).
123456
123456 PROCEDURE DIVISION.
123456     DISPLAY "START"
123456     OPEN INPUT TRANSACTS
123456     OPEN OUTPUT BALANCES
123456     READ TRANSACTS
123456       AT END
111111         WRITE BALANCE
123456       NOT AT END
123456         ADD 1 TO COUNTER
111111         IF FIRSTNAME OF TRANSACTION = FIRSTNAME OF BALANCE AND
000000            LASTNAME OF TRANSACTION = LASTNAME OF BALANCE THEN
000000              ADD TRANSACTION OF TRANSACTION TO SALDO OF BALANCE
000000         ELSE
111111           IF COUNTER NOT EQUAL 0 THEN
111111             WRITE BALANCE
111111           END-IF
111111           MOVE FIRSTNAME OF TRANSACTION TO FIRSTNAME OF BALANCE
111111           MOVE LASTNAME OF TRANSACTION TO LASTNAME OF BALANCE
111111           MOVE TRANSACTION OF TRANSACTION TO SALDO OF BALANCE
123456         END-IF
123456     END-READ
123456     CLOSE BALANCES
123456     CLOSE TRANSACTS
000000     STOP RUN.

        """.toIR(
            firPlugins = listOf(
                KeepNames(),
                NullableToZero(),
            ),
            irPlugins = listOf(
                Inlining(),
                BooleanExpressions(),
            ),
            fileConverter = {
                JavaFilesKotlin()
            },
            serialization = {
                KotlinxSerialization(it)
            },
            controlFlowHandling = {
                ExitProcessControlFlowHandlingFactory.ExitProcessControlFlowHandling
            },
        )

        val output = generate(input)

        //language=kotlin
        val expected =
"""package files

import app.softwork.serialization.flf.Ebcdic
import app.softwork.serialization.flf.Ebcdic.Format
import app.softwork.serialization.flf.FixedLength
import app.softwork.serialization.flf.FixedLengthFormat
import app.softwork.serialization.flf.append
import app.softwork.serialization.flf.decode
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import kotlin.Int
import kotlin.String
import kotlin.system.exitProcess
import kotlin.text.charset
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable

@ExperimentalSerializationApi
@Serializable
public data class TRANSACTION(
  @FixedLength(10)
  public val FIRSTNAME: String,
  @FixedLength(10)
  public val LASTNAME: String,
  @Ebcdic(Format.Zoned)
  @FixedLength(7)
  public val TRANSACTION: Int,
) {
  public companion object {
    public var FIRSTNAME: String = ""

    public var LASTNAME: String = ""

    public var TRANSACTION: Int = 0

    public fun create(): TRANSACTION {
      return TRANSACTION(FIRSTNAME, LASTNAME, TRANSACTION)
    }
  }
}

@ExperimentalSerializationApi
@Serializable
public data class BALANCE(
  @FixedLength(10)
  public val FIRSTNAME: String,
  @FixedLength(10)
  public val LASTNAME: String,
  @Ebcdic(Format.Zoned)
  @FixedLength(11)
  public val SALDO: Int,
) {
  public companion object {
    public var FIRSTNAME: String = ""

    public var LASTNAME: String = ""

    public var SALDO: Int = 0

    public fun create(): BALANCE {
      return BALANCE(FIRSTNAME, LASTNAME, SALDO)
    }
  }
}

public fun files() {
  var COUNTER: Int = 0
  println("START")
  val TRANSACTS: BufferedReader = File("TRANSACTS").bufferedReader(charset("IBM-1140"))
  val BALANCES: BufferedWriter = File("BALANCES").bufferedWriter(charset("IBM-1140"))
  for (TRANSACTION in TRANSACTS.decode(TRANSACTION.serializer(), FixedLengthFormat(""))) {
    COUNTER += 1
    if (TRANSACTION.FIRSTNAME == BALANCE.FIRSTNAME && TRANSACTION.LASTNAME == BALANCE.LASTNAME) {
      BALANCE.SALDO += TRANSACTION.TRANSACTION
    } else {
      if (COUNTER != 0) {
        BALANCES.append(BALANCE.serializer(), BALANCE.create())
      }
      BALANCE.FIRSTNAME = TRANSACTION.FIRSTNAME
      BALANCE.LASTNAME = TRANSACTION.LASTNAME
      BALANCE.SALDO = TRANSACTION.TRANSACTION
    }
  }
  BALANCES.append(BALANCE.serializer(), BALANCE.create())
  BALANCES.close()
  TRANSACTS.close()
  exitProcess(0)
}
"""
        assertEquals(expected, output.toString())
    }
}
