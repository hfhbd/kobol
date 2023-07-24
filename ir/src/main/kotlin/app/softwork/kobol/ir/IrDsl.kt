package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import kotlin.reflect.*

public fun function(
    name: String = "fake",
    parameters: List<Declaration> = emptyList(),
    private: Boolean = false,
    returnType: Type = Type.Natives.Void,
    block: Builder<Statement>.() -> Unit
): Function = Function(name = name, body = block, parameters = parameters,
    private = private,
    returnType = returnType
)

public operator fun Function.getValue(receiver: Any?, prop: KProperty<*>): Function = copy(name = prop.name)

public infix fun Statement.use(action: Statement): Use = Use(
    target = this,
    action = action,
    comments = emptyList()
)

public infix fun Callable.call(parameters: List<KobolIRTree.Expression>): FunctionCall =
    FunctionCall(declaration(), parameters)

public operator fun Callable.invoke(vararg parameters: KobolIRTree.Expression): FunctionCall = FunctionCall(
    declaration(), parameters.toList()
)

public val Int.l: IntLiteral get() = IntLiteral(this)
public val String.l: StringLiteral get() = StringLiteral(this)

public val testingReturnCodeIr: Declaration.IntDeclaration = Declaration.IntDeclaration.ReturnCode(
    name = "RETURN-CODE",
    const = false,
    mutable = true,
    isSigned = true,
    annotations = emptyMap(),
    length = 4,
    value = IntLiteral(0),
)

public fun Builder<Statement>.exit(): Exit = Exit(testingReturnCodeIr.variable() as KobolIRTree.Expression.NumberExpression.IntExpression, emptyList())
public val Builder<KobolIRTree.Types>.RC: Type.GlobalVariable get() = Type.GlobalVariable(testingReturnCodeIr, emptyList())
