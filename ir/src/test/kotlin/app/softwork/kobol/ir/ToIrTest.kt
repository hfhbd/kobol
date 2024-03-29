package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory.*
import java.io.File
import java.nio.file.Files
import kotlin.test.*
import app.softwork.kobol.ir.l as irL

class ToIrTest {
    @Test
    fun calling() {
        val fir = CobolFIRTree(
            fileName = "testing.cbl",
            id = ID("calling"),
            procedure = ProcedureTree(
                sections = buildList {
                    +Section(name = "FOO") {
                        +Display("FOO".l)
                        +Perform("BAR")
                        +Display("FOO2".l)
                        +StopRun()
                    }
                    +Section("BAR") {
                        +Display("BAR".l)
                        +StopRun()
                    }
                    +Section("C") {
                        +Display("C".l)
                    }
                },
            ),
        )

        val C by function {
            +Print(StringLiteral("C"))
        }
        val BAR by function {
            +Print(StringLiteral("BAR"))
            +exit()
        }

        val FOO by function(
            parameters = emptyList(),
            returnType = Natives.Void,
            private = false,
        ) {
            +Print(StringLiteral("FOO"))
            +BAR()
            +Print(StringLiteral("FOO2"), emptyList())
            +exit()
        }

        val ir = KobolIRTree(
            id = "testing.cbl",
            name = "calling",
            main = Function(
                name = "calling",
                parameters = emptyList(),
                returnType = Natives.Void,
                body = build {
                    +FOO()
                    +BAR()
                    +C()
                },
                private = false,
                doc = emptyList(),
                isEntryPoint = true,
            ),
            types = buildList {
                +FOO
                +BAR
                +C
                +RC
            },
        )
        assertEquals(
            expected = ir,
            actual = fir.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandling
                },
            ),
        )
    }

    @Test
    fun returnCodeFound() {
        //language=Cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 COUNTER  PIC 9(4).
        123456 PROCEDURE                   DIVISION.
        123456     MOVE 42 TO RETURN-CODE.
        """.trimIndent()

        val actual = input.toIR()

        val counter = Declaration.IntDeclaration.Normal(
            name = "COUNTER",
            length = 4,
            value = null,
            const = false,
            mutable = true,
            private = false,
            isSigned = false,
        )
        val expected = KobolIRTree(
            id = "testing.cbl",
            name = "hello",
            main = Function(
                name = "hello",
                parameters = emptyList(),
                returnType = Natives.Void,
                body = build {
                    +Assignment(testingReturnCodeIr, 42.irL)
                },
                private = false,
                doc = emptyList(),
                isEntryPoint = true,
            ),
            types = buildList {
                +GlobalVariable(counter, emptyList())
                +RC
            },
        )
        assertEquals(expected, actual)
    }
}

private fun String.toIR(): KobolIRTree {
    val temp = Files.createTempDirectory("testing")
    return listOf(
        File(temp.toFile(), "kobol.cbl").apply { writeText(this@toIR) },
    ).toIR(temp).single().copy(id = "testing.cbl")
}
