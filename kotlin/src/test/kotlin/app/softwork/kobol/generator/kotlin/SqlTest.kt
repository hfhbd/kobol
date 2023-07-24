package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.sqldelightprecompiler.*
import kotlin.test.*

class SqlTest {
    @Test
    fun selectInto() {
        lateinit var sqlPrecompiler: SqlDelightPrecompiler
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
        123456 SELECT AVG(42, :BAR), 42 
        123456  INTO :FOO, :BAZ 
        123456 FROM SYSIBM.SYSDUMMY1;
        123456 END-EXEC
        123456 DISPLAY FOO
        123456 DISPLAY BAR
        123456 DISPLAY BAZ.
        """.trimIndent().toIR(sqlPrecompiler = { packageName, folder ->
            sqlPrecompiler = SqlDelightPrecompiler("DB", folder, packageName, packageName)
            sqlPrecompiler
        })

        val output = generate(input)

        //language=kotlin
        val expected = """
        package sql
        
        import kotlin.Int
        
        public var FOO: Int? = null
        
        public var BAR: Int = 10
        
        public var BAZ: Int? = null
        
        public fun sql() {
          val db: DB = DB(driver)
          /**
           * Get AVG of 42
           */
          val selectAvg42bar42intofoobazFromSysibmsysdummy1: SelectAvg42bar42intofoobazFromSysibmsysdummy1 =
              db.sqlQueries.selectAvg42bar42intofoobazFromSysibmsysdummy1(BAR).executeAsOne()
          FOO = selectAvg42bar42intofoobazFromSysibmsysdummy1.FOO
          BAZ = selectAvg42bar42intofoobazFromSysibmsysdummy1.BAZ
          println(FOO)
          println(BAR)
          println(BAZ)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())

        assertEquals(
            """
            |/**
            | * Get AVG of 42
            | */
            |selectAvg42bar42intofoobazFromSysibmsysdummy1:
            |SELECT AVG(42, :BAR), 42
            | INTO :FOO, :BAZ
            |FROM SYSIBM.SYSDUMMY1;
            |
        """.trimMargin(), sqlPrecompiler.files!!.queries.single().toString()
        )
    }

    @Test
    fun include() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION DIVISION.
        123456 PROGRAM-ID. SQL.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 EXEC SQL INCLUDE INCLUDETESTING END-EXEC.
        123456 PROCEDURE DIVISION.
        123456 DISPLAY BAR OF FOO.
        """.trimIndent().toIR(
            Triple(null, "INCLUDETESTING", """
            |01 FOO.
            |123456 02 BAR PIC 9(2).
            |       02 ABB PIC 9(2).
            |
            """.trimMargin()),
            sqlPrecompiler = { packageName, folder ->
                SqlDelightPrecompiler("DB", folder, packageName, packageName)
            }
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package sql
        
        import kotlin.Int
        
        public object FOO {
          public var BAR: Int? = null
        
          public var ABB: Int? = null
        }
        
        public fun sql() {
          println(FOO.BAR)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun createTable() {
        lateinit var sqlPrecompiler: SqlDelightPrecompiler
        //language=cobol
        val input = """
        123456 IDENTIFICATION DIVISION.
        123456 PROGRAM-ID. SQL.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 EXEC SQL INCLUDE SQLCA END-EXEC.
        123456* TABLE COMMENT
        123456* TABLE COMMENT II
        123456 EXEC SQL
        123456 CREATE TABLE foo(
        123456  id INTEGER,
        123456  a INTEGER
        123546 );
        123456 CREATE TABLE bar(
        123456  id INTEGER,
        123456  a INTEGER
        123546 );
        123456 END-EXEC.
        123456 EXEC SQL
        123456 CREATE TABLE baz(
        123456  id INTEGER,
        123456  a INTEGER
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
        """.trimIndent().toIR(sqlPrecompiler = { packageName, folder ->
            sqlPrecompiler = SqlDelightPrecompiler("DB", folder, packageName, packageName)
            sqlPrecompiler
        })

        val output = generate(input)

        //language=kotlin
        val expected = """
        package sql
        
        import kotlin.Int
        import kotlin.String
        
        public object SQLCA {
          public var SQLCAID: String = "SQLCA   "
        
          public var SQLCABC: Int = 136
        
          public var SQLCODE: Int? = null
        
          public var SQLERRML: Int? = null
        
          public var SQLERRMC: String? = null
        
          public var SQLERRP: String? = null
        
          public var SQLERRD: Int? = null
        
          public var SQLWARN0: String? = null
        
          public var SQLWARN1: String? = null
        
          public var SQLWARN2: String? = null
        
          public var SQLWARN3: String? = null
        
          public var SQLWARN4: String? = null
        
          public var SQLWARN5: String? = null
        
          public var SQLWARN6: String? = null
        
          public var SQLWARN7: String? = null
        
          public var SQLWARN8: String? = null
        
          public var SQLWARN9: String? = null
        
          public var SQLWARNA: String? = null
        
          public var SQLSTATE: String? = null
        }
        
        public var FOO: Int? = null
        
        public var BAR: Int? = null
        
        public fun sql() {
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
              db.sqlQueries.selectIdAIntofoobarFromFoo().executeAsOne()
          FOO = selectIdAIntofoobarFromFoo.FOO
          BAR = selectIdAIntofoobarFromFoo.BAR
          val selectIdAIntofoobarFromFoo_: SelectIdAIntofoobarFromFoo_ =
              db.sqlQueries.selectIdAIntofoobarFromFoo_().executeAsOne()
          FOO = selectIdAIntofoobarFromFoo_.FOO
          BAR = selectIdAIntofoobarFromFoo_.BAR
          val setfoobarSelectIdAFromFoo: SetfoobarSelectIdAFromFoo =
              db.sqlQueries.setfoobarSelectIdAFromFoo().executeAsOne()
          FOO = setfoobarSelectIdAFromFoo.FOO
          BAR = setfoobarSelectIdAFromFoo.BAR
          db.sqlQueries.insertIntoFooValuesfoobar(FOO, BAR)
          println(FOO)
          println(BAR)
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())

        assertEquals(
            // language=db2_zos
            """
            |/**
            | * TABLE COMMENT
            | * TABLE COMMENT II
            | */
            |CREATE TABLE foo(
            | id INTEGER,
            | a INTEGER
            |);
            |
            |CREATE TABLE bar(
            | id INTEGER,
            | a INTEGER
            |);
            |
            |CREATE TABLE baz(
            | id INTEGER,
            | a INTEGER
            |);
            |
        """.trimMargin(), sqlPrecompiler.files!!.migrations.single().toString()
        )

        assertEquals(
            // language=db2_zos
            """
            |/**
            | * INSERT COMMENT
            | * INSERT COMMENT II
            | */
            |insertIntoFooValues12:
            |INSERT INTO foo VALUES (1, 2);
            |
            |/**
            | * COMMENT
            | * COMMENT II
            | */
            |selectIdAIntofoobarFromFoo:
            |SELECT id, a INTO :FOO, :BAR FROM foo;
            |
            |selectIdAIntofoobarFromFoo_:
            |SELECT id, a INTO :FOO, :BAR FROM foo;
            |
            |setfoobarSelectIdAFromFoo:
            |SET :FOO, :BAR = SELECT id, a FROM foo;
            |
            |insertIntoFooValuesfoobar:
            |INSERT INTO foo VALUES (:FOO, :BAR);
            |
        """.trimMargin(), sqlPrecompiler.files!!.queries.single().toString()
        )
    }
}
