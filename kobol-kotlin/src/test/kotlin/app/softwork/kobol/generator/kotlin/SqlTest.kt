package app.softwork.kobol.generator.kotlin

import kotlin.test.*

class SqlTest {
    @Test
    fun selectInto() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION DIVISION.
        123456 PROGRAM-ID. SQL.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 FOO PIC 9(2).
        123456 77 BAR PIC 9(2) VALUE 10.
        123456 77 BAZ PIC 9(2).
        123456 PROCEDURE DIVISION.
        123456* Get AVG of 42
        123456 EXEC SQL
        123456   SELECT AVG(42, :BAR), 42 INTO :FOO, :BAZ FROM SYSIBM.SYSDUMMY1;
        123456 END-EXEC
        123456 DISPLAY FOO
        123456 DISPLAY BAR
        123456 DISPLAY BAZ.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package sql
        
        import `selectAvg(42bar)42IntofoobazFromSysibmsysdummy1`
        import kotlin.Int
        import kotlin.Unit
        
        public var FOO: Int? = null
        
        public var BAR: Int = 10
        
        public var BAZ: Int? = null
        
        public fun main(): Unit {
          /**
           * Get AVG of 42
           */
          val `selectAvg(42bar)42IntofoobazFromSysibmsysdummy1`:
              `selectAvg(42bar)42IntofoobazFromSysibmsysdummy1` =
              sql.sqQueries.`selectAvg(42bar)42IntofoobazFromSysibmsysdummy1`(BAR)
          .executeAsOne()
        
          FOO = `selectAvg(42bar)42IntofoobazFromSysibmsysdummy1`.FOO
          BAZ = `selectAvg(42bar)42IntofoobazFromSysibmsysdummy1`.BAZ
          println(FOO)
          println(BAR)
          println(BAZ)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
