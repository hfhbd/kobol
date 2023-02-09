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
    block: Builder<Statement>.() -> Unit
): Function = Function(name = name, body = block, parameters = parameters)

public operator fun Function.getValue(receiver: Any?, prop: KProperty<*>): Function = copy(name = prop.name)

public infix fun Statement.use(action: Statement): Use = Use(
    target = this,
    action = action,
    comments = emptyList()
)

public infix fun Callable.call(parameters: List<KobolIRTree.Expression>): FunctionCall =
    FunctionCall(this, parameters)

public operator fun Callable.invoke(vararg parameters: KobolIRTree.Expression): FunctionCall = FunctionCall(
    this, parameters.toList()
)

public val Int.l: IntLiteral get() = IntLiteral(this)
public val String.l: StringLiteral get() = StringLiteral(this)
