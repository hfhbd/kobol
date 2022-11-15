package app.softwork.kobol.ir

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.KobolIRTree.Types.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import kotlin.reflect.*

public fun function(
    name: String = "fake",
    block: Builder<Statement>.() -> Unit
): Function = Function(name = name, body = block)

public operator fun Function.getValue(receiver: Any?, prop: KProperty<*>): Function = copy(name = prop.name)

public infix fun Statement.use(action: Statement): Use = Use(
    target = this,
    action = action,
    comments = emptyList()
)

public infix fun Callable.call(parameters: List<KobolIRTree.Expression>): FunctionCall =
    FunctionCall(this, parameters)
