package app.softwork.kobol.ir

import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.StringLiteral
import app.softwork.kobol.ir.KobolIRTree.Types.Callable
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Exit
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.FunctionCall
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Use
import app.softwork.kobol.ir.KobolIRTree.Types.Type
import kotlin.reflect.KProperty

public fun function(
    name: String = "fake",
    packageName: String? = null,
    parameters: List<Declaration> = emptyList(),
    isStatic: Boolean = true,
    private: Boolean = false,
    topLevel: Boolean = false,
    returnType: Type = Type.Natives.Void,
    block: MutableList<Statement>.() -> Unit,
): Function = Function(
    name = name,
    packageName = packageName,
    topLevel = topLevel,
    body = block,
    parameters = parameters,
    isStatic = isStatic,
    private = private,
    returnType = returnType,
)

public operator fun Function.getValue(receiver: Any?, prop: KProperty<*>): Function = copy(name = prop.name)

public fun klass(
    name: String = "fake",
    packageName: String,
    functions: MutableList<Function>.() -> Unit = {},
    constructor: MutableList<Declaration>.() -> Unit = {},
): Type.Class = Type.Class(
    name = name,
    packageName = packageName,
    functions = buildList(functions),
    constructor = buildList(constructor),
)

public operator fun Type.Class.getValue(receiver: Any?, prop: KProperty<*>): Type.Class = copy(name = prop.name)

public infix fun Statement.use(action: Statement): Use = Use(
    target = this,
    action = action,
    comments = emptyList(),
)

public infix fun Callable.call(parameters: List<KobolIRTree.Expression>): FunctionCall =
    FunctionCall(declaration(), parameters)

public operator fun Callable.invoke(vararg parameters: KobolIRTree.Expression): FunctionCall = FunctionCall(
    declaration(),
    parameters.toList(),
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

public fun MutableList<Statement>.exit(): Exit = Exit(
    testingReturnCodeIr.variable() as KobolIRTree.Expression.NumberExpression.IntExpression,
    emptyList(),
)

public val MutableList<KobolIRTree.Types>.RC: Type.GlobalVariable
    get() = Type.GlobalVariable(
        testingReturnCodeIr,
        emptyList(),
    )
