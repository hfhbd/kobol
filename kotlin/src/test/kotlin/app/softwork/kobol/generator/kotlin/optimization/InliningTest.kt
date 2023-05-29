package app.softwork.kobol.generator.kotlin.optimization

import app.softwork.kobol.generator.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.fir.*
import app.softwork.kobol.plugins.fir.renaming.*
import app.softwork.kobol.plugins.ir.optimizations.*
import org.intellij.lang.annotations.*
import kotlin.test.*

class InliningTest {
    @Test
    fun inlining() {
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
        123456     DISPLAY HELLO WORLD
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .let { Inlining()(it, emptyList()) }.single()

        val output = generate(input)

        @Language("kotlin")
        val expected = """
        package hello
        
        import kotlin.String
        
        public fun main() {
          val HELLO: String = "HELLO"
          var WORLD: String = "WORLD!"
          println("${'$'}HELLO${'$'}WORLD")
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Ignore
    @Test
    fun functionsWithDCE() {
        //language=cobol
        val input = """
        |000010 IDENTIFICATION DIVISION.
        |000020 PROGRAM-ID.                 FILES.
        |000030 ENVIRONMENT DIVISION.
        |000040 INPUT-OUTPUT SECTION.
        |000050 FILE-CONTROL.
        |000060     SELECT TRANSACTIONS     ASSIGN T
        |000070                             FILE STATUS T-STATUS.
        |000080     SELECT BALANCES         ASSIGN B
        |000090                             FILE STATUS B-STATUS.
        |000100 DATA DIVISION.
        |000110 FILE SECTION.
        |000120 FD  TRANSACTIONS
        |000130     RECORDING               V
        |000140     LABEL RECORD            STANDARD
        |000150     DATA RECORD             EB1-EIN.
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
        |123456         ADD 1 TO COUNT
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
            irPlugins = listOf(Inlining()),
            fileConverter = { JavaFilesKotlin() },
            serialization = { KotlinxSerialization(it) }
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package files
        
        import app.softwork.serialization.flf.FixedLength
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
          @FixedLength(6)
          public val TRANSACTION: Int,
        )
        
        @ExperimentalSerializationApi
        @Serializable
        public data class BALANCE(
          @FixedLength(10)
          public val FIRSTNAME: String,
          @FixedLength(10)
          public val LASTNAME: String,
          @FixedLength(6)
          public val BALANCE: Int,
        )
        
        public fun main() {
          var COUNT: Int = 0
          var FIRSTNAME: String = ""
          var LASTNAME: String = ""
          var BALANCE: Int = 0
          val TRANSACTIONS: BufferedReader = File("TRANSACTIONS").bufferedReader(charset("IBM-1140"))
          val BALANCES: BufferedWriter = File("BALANCES").bufferedWriter(charset("IBM-1140"))
          for (TRANSACTION in TRANSACTIONS.lineSequence().decode(TRANSACTION.serializer())) {
            COUNT += 1
            if (TRANSACTION.FIRSTNAME == FIRSTNAME && TRANSACTION.LASTNAME == LASTNAME) {
              BALANCE += TRANSACTION.TRANSACTION
            } else {
              BALANCES.append(BALANCE.serializer(), BALANCE(FIRSTNAME, LASTNAME, BALANCE))
              FIRSTNAME = TRANSACTION.FIRSTNAME
              LASTNAME = TRANSACTION.LASTNAME
              BALANCE = TRANSACTION.TRANSACTION
            }
            BALANCES.append(BALANCE.serializer(), BALANCE(FIRSTNAME, LASTNAME, BALANCE))
          }
          println("COUNT ${'$'}COUNT")
          BALANCES.close()
          TRANSACTIONS.close()
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
