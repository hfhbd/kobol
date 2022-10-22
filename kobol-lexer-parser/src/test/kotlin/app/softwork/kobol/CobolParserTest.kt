package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.Formatter.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.Formatter.Custom.Part.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.Compressed.*
import app.softwork.kobol.CobolFIRTree.EnvTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.BooleanExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.NumberExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.Sql.SqlType.*
import org.intellij.lang.annotations.*
import kotlin.test.*

class CobolParserTest {
    @Test
    fun data() {
        //language=COBOL
        val input = """
                123456 IDENTIFICATION DIVISION.
                123456 PROGRAM-ID. HELLO.
                123456 DATA DIVISION.
                123456 FILE SECTION.
                123456 FD FOO
                123456     RECORDING               V
                123456     LABEL RECORD            STANDARD
                123456     DATA RECORD             BAR.
                123456 01 BAR.
                123456    05 FOO PIC X(3).
                123456    05 BAR PIC A(3).            
                123456 WORKING-STORAGE SECTION.
                123456 01 RPI.
                123456 05 WORLD PIC X(6) VALUE 'WORLD!'.
                123456 05 ANSWER PIC A(6).
                123456 77 FOO PIC X(6) VALUE '123456'.
                123456 01 RPICA.
                123456    05 FOO PIC A(3).
                123456 77 BAR PIC A(3).
                123456 PROCEDURE DIVISION.
                123456 DISPLAY WORLD
                123456 DISPLAY FOO
                123456 DISPLAY FOO OF RPICA.
            """.trimIndent()
        val world = StringElementar(name = "WORLD", recordName = "RPI", value = "WORLD!", formatter = Simple(6))
        val foo = StringElementar(name = "FOO", recordName = null, value = "123456", formatter = Simple(6))
        val fooRPICA = StringElementar(name = "FOO", recordName = "RPICA", formatter = Simple(3))

        assertEquals(CobolFIRTree(id = CobolFIRTree.ID(
            programID = "HELLO"
        ), data = CobolFIRTree.DataTree(workingStorage = build {
            +Record(name = "RPI") {
                +world
                +StringElementar(name = "ANSWER", recordName = "RPI", formatter = Simple(6))
            }
            +foo
            +Record(name = "RPICA") {
                +fooRPICA
            }
            +StringElementar(name = "BAR", recordName = null, formatter = Simple(3))
        }), procedure = CobolFIRTree.ProcedureTree(topLevel = build {
            +Display(StringVariable(target = world))
            +Display(StringVariable(target = foo))
            +Display(StringVariable(target = fooRPICA))
        })
        ), input.toTree()
        )
    }

    @Test
    fun testInput() {
        @Language("Cobol") val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456* Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()

        val world = StringElementar(name = "WORLD", recordName = null, value = "WORLD!", formatter = Simple(6))
        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ),
                data = CobolFIRTree.DataTree(workingStorage = listOf(world)),
                procedure = CobolFIRTree.ProcedureTree(topLevel = build {
                    +Display(
                        Concat(
                            StringLiteral("HELLO"), StringVariable(target = world)
                        ), comments = listOf("Some Comment")
                    )
                    +Move(value = StringLiteral("42"), target = world)
                    +Display(
                        StringLiteral("ANSWER") + StringVariable(target = world)
                    )
                })
            ), input.toTree()
        )
    }

    @Test
    fun specialNames() {
        @Language("Cobol") val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 ENVIRONMENT DIVISION.
            123456 CONFIGURATION SECTION.
            123456 SPECIAL-NAMES.
            123456     DECIMAL-POINT           IS COMMA.
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO"
            123456     GOBACK.
        """.trimIndent()

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ), env = CobolFIRTree.EnvTree(
                    configuration = Configuration(
                        specialNames = Configuration.SpecialNames(
                            specialNames = listOf(
                                Configuration.SpecialNames.SpecialName("DECIMAL-POINT", "COMMA")
                            )
                        )
                    )
                ), procedure = CobolFIRTree.ProcedureTree(topLevel = build {
                    +Display(StringLiteral("HELLO"))
                    +GoBack()
                })
            ), input.toTree()
        )
    }

    @Test
    fun fileConfig() {
        @Language("Cobol") val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 ENVIRONMENT DIVISION.
            123456* INPUT I
            123456* INPUT II
            123456 INPUT-OUTPUT SECTION.
            123456* FILE I
            123456* FILE II
            123456 FILE-CONTROL.
            123456* FOO I
            123456* FOO II
            123456     SELECT FOO-FILE ASSIGN FOO FILE STATUS FOO-STATUS.
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO".
        """.trimIndent()

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ), env = CobolFIRTree.EnvTree(
                    inputOutput = InputOutput(
                        fileControl = InputOutput.FileControl(
                            files = listOf(
                                InputOutput.FileControl.File(
                                    file = "FOO-FILE",
                                    fileVariable = "FOO",
                                    fileStatus = "FOO-STATUS",
                                    comments = listOf("FOO I", "FOO II")
                                )
                            ), comments = listOf("FILE I", "FILE II")
                        ), comments = listOf("INPUT I", "INPUT II")
                    )
                ), procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(StringLiteral("HELLO"))
                    )
                )
            ), input.toTree()
        )
    }

    @Test
    fun records() {
        //language=cobol
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 01 RPI.
            123456 05 FOO1.
            123456 10 WORLD2 PIC 9 VALUE 9.
            123456 15 WORLD3 POINTER.
            123456 20 WORLD4 PIC X(6) VALUE 'WORLD!' OCCURS 2.
            123456 25 FOO5 PIC 9(9) VALUE 123456 OCCURS 9 TO 9 DEPENDING ON WORLD2.
            123456 30      PIC 9(2).
            123456 77 WORLD6 PIC A.
            123456 77 FOO7 PIC S9(6)V9 VALUE .9.
            123456 77 FOO8 PIC V9 VALUE .9.
            123456 01 RPICA.
            123456    05 FOOPIC PIC 9(3).
            123456 77 FOO9 PIC 9(3).
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO"WORLD4.
        """.trimIndent()
        val world4 =
            StringElementar("WORLD4", recordName = "RPI", formatter = Simple(6), value = "WORLD!", occurs = Occurs(2))
        assertEquals(CobolFIRTree(id = CobolFIRTree.ID(
            programID = "HELLO", author = "WEDEMANN / Softwork.app"
        ), data = CobolFIRTree.DataTree(workingStorage = build {
            +Record("RPI") {
                +EmptyElementar("FOO1", "RPI")
                val world2 = NumberElementar("WORLD2", recordName = "RPI", formatter = Simple(1), value = 9.0)
                +world2
                +Pointer("WORLD3", recordName = "RPI")
                +world4
                +NumberElementar(
                    "FOO5",
                    recordName = "RPI",
                    formatter = Simple(9),
                    value = 123456.0,
                    occurs = Occurs(9, 9, dependingOn = world2)
                )
            }

            +StringElementar("WORLD6", recordName = null, formatter = Simple(1))
            +NumberElementar(
                "FOO7", recordName = null, formatter = Custom(Signed(6), Decimal(1)), value = .9, signed = true
            )
            +NumberElementar("FOO8", recordName = null, formatter = Simple(1), value = .9, signed = true)
            +Record("RPICA") {
                +NumberElementar("FOOPIC", recordName = "RPICA", formatter = Simple(3))
            }
            +NumberElementar("FOO9", recordName = null, formatter = Simple(3))
        }), procedure = CobolFIRTree.ProcedureTree(topLevel = build {
            +Display(StringLiteral("HELLO") + StringVariable(world4))
        })
        ), input.toTree()
        )
    }

    @Test
    fun perform() {
        //language=cobol
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD2 PIC 9 VALUE 9.
            123456 PROCEDURE                   DIVISION.
            123456 PERFORM FOO UNTIL WORLD2 = 2
            123456 PERFORM UNTIL WORLD2 = 2 
            123456     PERFORM FOO
            123456 END-PERFORM.
            123456 PERFORM VARYING WORLD2 FROM 1 BY 3 UNTIL WORLD2 = 4
            123456     PERFORM FOO
            123456 END-PERFORM.
            123456 FOO SECTION.
            123456 DISPLAY 'FOO'.
        """.trimIndent()

        val world2 = NumberElementar("WORLD2", recordName = null, formatter = Simple(1), value = 9.0)

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(programID = "HELLO"),
                data = CobolFIRTree.DataTree(workingStorage = build {
                    +world2
                }),
                procedure = CobolFIRTree.ProcedureTree(topLevel = build {
                    +Perform("FOO", until = Equals(NumberVariable(world2), 2.l))
                    +While(build {
                        +Perform("FOO")
                    }, until = NumberVariable(world2) eq 2.l)
                    +ForEach(variable = world2,
                        from = 1.l,
                        by = 3.l,
                        until = NumberVariable(world2) eq 4.l,
                        statements = build {
                            +Perform("FOO")
                        })
                }, sections = listOf(CobolFIRTree.ProcedureTree.Section("FOO") {
                    +Display("FOO".l)
                }))
            ), input.toTree()
        )
    }

    @Test
    fun sql() {
        //language=cobol
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 BAR PIC 9 VALUE 1.
            123456 77 BARRESULT PIC 9 VALUE 1.
            123456       EXEC SQL
            123456         INCLUDE SQLCA
            123456       END-EXEC.
            123456       EXEC SQL
            123456         INCLUDE LINES
            123456       END-EXEC.
            123456 PROCEDURE                   DIVISION.
            123456 EXEC SQL
            123456 SELECT 42 INTO :FOO FROM SYSIBM.SYSDUMMY1;
            123456 SET :FOO = SELECT 42 FROM SYSIBM.SYSDUMMY1;
            123456 SET :FOO = 42;
            123456 END-EXEC
            123456
            123456 EXEC SQL
            123456 SELECT 42 AS f INTO :FOO FROM SYSIBM.SYSDUMMY1 WHERE f = 42 ORDER BY f DESC;
            123456 SELECT 42 AS f, :BAR INTO :FOO, :BARRESULT FROM SYSIBM.SYSDUMMY1 WHERE f = 42 AND :FOO IS 1 ORDER BY f DESC;
            123456 END-EXEC
            123456 DISPLAY FOO 
            123456 DISPLAY SQLSTATE.
        """.trimIndent()

        val sqlca = """
            01 SQLCA SYNC.
                05 SQLCAID PIC X(8) VALUE "SQLCA   ".
                05 SQLCABC PIC S9(9) COMP-5 VALUE 136.
                05 SQLCODE PIC S9(9) COMP-5.
                05 SQLERRM.
                05 SQLERRP PIC X(8).
                05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
                05 SQLWARN.
                    10 SQLWARN0 PIC X.
                    10 SQLWARN1 PIC X.
                    10 SQLWARN2 PIC X.
                    10 SQLWARN3 PIC X.
                    10 SQLWARN4 PIC X.
                    10 SQLWARN5 PIC X.
                    10 SQLWARN6 PIC X.
                    10 SQLWARN7 PIC X.
                    10 SQLWARN8 PIC X.
                    10 SQLWARN9 PIC X.
                    10 SQLWARNA PIC X.
                05 SQLSTATE PIC X(5).
        """.trimIndent()

        val lines = """
            123456 77 FOO PIC 9 VALUE 1.
        """.trimIndent()

        val sqlState = StringElementar("SQLSTATE", "SQLCA", Simple(5))
        val foo = NumberElementar("FOO", recordName = null, value = 1.0, formatter = Simple(1))
        val bar = NumberElementar("BAR", recordName = null, value = 1.0, formatter = Simple(1))
        val barResult = NumberElementar("BARRESULT", recordName = null, value = 1.0, formatter = Simple(1))

        assertEquals(CobolFIRTree(id = CobolFIRTree.ID(programID = "HELLO"),
            data = CobolFIRTree.DataTree(workingStorage = build {
                +bar
                +barResult
                +Record("SQLCA") {
                    +StringElementar("SQLCAID",  recordName = "SQLCA",value = "SQLCA   ", formatter = Simple(8))
                    +NumberElementar(
                        "SQLCABC",  recordName = "SQLCA",value = 136.0, signed = true, compressed = COMP5, formatter = Simple(9)
                    )
                    +NumberElementar("SQLCODE",  recordName = "SQLCA",signed = true, compressed = COMP5, formatter = Simple(9))
                    +EmptyElementar("SQLERRM", recordName = "SQLCA")
                    +StringElementar("SQLERRP", recordName = "SQLCA", Simple(8))
                    +NumberElementar("SQLERRD", recordName = "SQLCA", Simple(9), signed = true, compressed = COMP5, occurs = Occurs(6))
                    +EmptyElementar("SQLWARN", recordName = "SQLCA")
                    repeat(10) {
                        +StringElementar("SQLWARN$it", recordName = "SQLCA", Simple(1))
                    }
                    +StringElementar("SQLWARNA", recordName = "SQLCA", Simple(1))
                    +sqlState
                }
                +foo
            }),
            procedure = CobolFIRTree.ProcedureTree(topLevel = build {
                +CobolFIRTree.ProcedureTree.Statement.Sql(
                    "SELECT 42 INTO :FOO FROM SYSIBM.SYSDUMMY1",
                    hostVariables = listOf(NumberVariable(foo)),
                    parameter = emptyList(),
                    type = Select
                )
                +CobolFIRTree.ProcedureTree.Statement.Sql(
                    "SET :FOO = SELECT 42 FROM SYSIBM.SYSDUMMY1",
                    hostVariables = listOf(NumberVariable(foo)),
                    parameter = emptyList(),
                    type = Execute
                )
                +CobolFIRTree.ProcedureTree.Statement.Sql(
                    "SET :FOO = 42",
                    hostVariables = listOf(NumberVariable(foo)),
                    parameter = emptyList(),
                    type = Execute
                )
                +CobolFIRTree.ProcedureTree.Statement.Sql(
                    "SELECT 42 AS f INTO :FOO FROM SYSIBM.SYSDUMMY1 WHERE f = 42 ORDER BY f DESC",
                    hostVariables = listOf(NumberVariable(foo)),
                    parameter = listOf(),
                    type = Select
                )
                +CobolFIRTree.ProcedureTree.Statement.Sql(
                    "SELECT 42 AS f, :BAR INTO :FOO, :BARRESULT FROM SYSIBM.SYSDUMMY1 WHERE f = 42 AND :FOO IS 1 ORDER BY f DESC",
                    hostVariables = listOf(NumberVariable(foo), NumberVariable(barResult)),
                    parameter = listOf(NumberVariable(bar), NumberVariable(foo)),
                    type = Select
                )
                +Display(Interpolation(NumberVariable(foo)))
                +Display(StringVariable(sqlState))
            })
        ), input.toTree("SQLCA" to sqlca, "LINES" to lines)
        )
    }

    private infix fun CobolFIRTree.ProcedureTree.Expression.eq(right: CobolFIRTree.ProcedureTree.Expression) =
        Equals(this, right)
}
