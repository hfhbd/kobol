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
        
        import kotlin.Int
        import kotlin.Unit
        
        public var FOO: Int? = null
        
        public var BAR: Int = 10
        
        public var BAZ: Int? = null
        
        public fun main(): Unit {
          val db: DB = DB(driver)
          /**
           * Get AVG of 42
           */
          val selectAvg42bar42IntofoobazFromSysibmsysdummy1: SelectAvg42bar42IntofoobazFromSysibmsysdummy1 =
              db.sqlQueries.selectAvg42bar42IntofoobazFromSysibmsysdummy1(BAR)
          .executeAsOne()
        
          FOO = selectAvg42bar42IntofoobazFromSysibmsysdummy1.FOO
          BAZ = selectAvg42bar42IntofoobazFromSysibmsysdummy1.BAZ
          println(FOO)
          println(BAR)
          println(BAZ)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun createTable() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION DIVISION.
        123456 PROGRAM-ID. SQL.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456* TABLE COMMENT
        123456* TABLE COMMENT II
        123456 EXEC SQL
        123456 CREATE TABLE foo(
        123456 id INTEGER,
        123456 a INTEGER
        123546 );
        123456 END-EXEC.
        123456 77 FOO PIC 9(2).
        123456 77 BAR PIC 9(2).
        123456 PROCEDURE DIVISION.
        123456* INSERT COMMENT
        123456* INSERT COMMENT II
        123456 EXEC SQL
        123456 INSERT INTO foo VALUES (1, 2);
        123456 END-EXEC
        123456* COMMENT
        123456* COMMENT II
        123456 EXEC SQL
        123456   SELECT id, a INTO :FOO, :BAR FROM foo;
        123456 END-EXEC
        123456 EXEC SQL
        123456   SELECT id, a INTO :FOO, :BAR FROM foo;
        123456 END-EXEC
        123456 EXEC SQL
        123456   SET :FOO, :BAR = SELECT id, a FROM foo;
        123456 END-EXEC
        123456 EXEC SQL
        123456   INSERT INTO foo VALUES (:FOO, :BAR);
        123456 END-EXEC
        123456 DISPLAY FOO
        123456 DISPLAY BAR.
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package sql
        
        import kotlin.Int
        import kotlin.Unit
        
        public var FOO: Int? = null
        
        public var BAR: Int? = null
        
        public fun main(): Unit {
          val db: DB = DB(driver)
          DB.Schema.migrate(driver, 0, 1)
          // INSERT COMMENT
          // INSERT COMMENT II
          db.sqlQueries.insertIntoFooValues12()
          /**
           * COMMENT
           * COMMENT II
           */
          val selectIdAIntofoobarFromFoo: SelectIdAIntofoobarFromFoo =
              db.sqlQueries.selectIdAIntofoobarFromFoo()
          .executeAsOne()
        
          FOO = selectIdAIntofoobarFromFoo.FOO
          BAR = selectIdAIntofoobarFromFoo.BAR
          val selectIdAIntofoobarFromFoo_: SelectIdAIntofoobarFromFoo_ =
              db.sqlQueries.selectIdAIntofoobarFromFoo_()
          .executeAsOne()
        
          FOO = selectIdAIntofoobarFromFoo_.FOO
          BAR = selectIdAIntofoobarFromFoo_.BAR
          val setfoobarSelectIdAFromFoo: SetfoobarSelectIdAFromFoo =
              db.sqlQueries.setfoobarSelectIdAFromFoo()
          .executeAsOne()
        
          FOO = setfoobarSelectIdAFromFoo.FOO
          BAR = setfoobarSelectIdAFromFoo.BAR
          db.sqlQueries.insertIntoFooValuesfoobar(FOO, BAR)
          println(FOO)
          println(BAR)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
