package app.softwork.kobol.ir

import app.softwork.kobol.Builder
import app.softwork.kobol.build
import app.softwork.kobol.ir.KobolIRTree.Expression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.DoubleExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import kotlinx.serialization.Serializable
import java.lang.IllegalStateException

/**
 * Never change the [id]
 */
@Serializable
public data class KobolIRTree(
    val id: String,
    val name: String,
    val packageName: String? = null,
    val main: Types.Function,
    val types: List<Types>
) {
    @Serializable
    public sealed interface Types {
        @Serializable
        public sealed interface Callable {
            public fun declaration(): Callable
        }

        @Serializable
        public data class Function(
            val name: String,
            val parameters: List<Declaration> = emptyList(),
            val returnType: Type,
            val body: MutableList<Statement>,
            val private: Boolean = false,
            val doc: List<String> = emptyList(),
            val external: Boolean = false,
            val topLevel: Boolean = false,
            val packageName: String? = null,
            val inlineWith: Statement.FunctionCall? = null,
            val isStatic: Boolean = true,
            val isEntryPoint: Boolean = false,
        ) : Types, Callable {

            public override fun declaration(): Function = copy(body = mutableListOf(), doc = emptyList())

            public constructor(
                name: String,
                parameters: List<Declaration> = emptyList(),
                returnType: Type = Type.Natives.Void,
                private: Boolean = false,
                doc: List<String> = emptyList(),
                external: Boolean = false,
                topLevel: Boolean = false,
                packageName: String? = null,
                isStatic: Boolean = true,
                body: Builder<Statement>.() -> Unit
            ) : this(
                name = name,
                parameters = parameters,
                returnType = returnType,
                body = build(body),
                private = private,
                doc = doc,
                external = external,
                topLevel = topLevel,
                packageName = packageName,
                isStatic = isStatic
            )

            @Serializable
            public sealed interface Statement {

                public val comments: List<String>

                @Serializable
                public sealed interface Declaration : Statement {
                    public val name: String
                    public val mutable: Boolean
                    public val private: Boolean
                    public val value: Expression?
                    public val nullable: Boolean
                    public val annotations: Map<String, List<Expression>>

                    @Serializable
                    public data class Array(
                        override val name: String,
                        val type: Type,
                        override val nullable: Boolean,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val annotations: Map<String, List<Expression>> = emptyMap()
                    ) : Declaration {
                        override val value: Nothing? = null
                    }

                    @Serializable
                    public sealed interface Primitive : Declaration {
                        public val const: Boolean
                        public val length: Int
                        public val synthetic: Boolean
                    }

                    @Serializable
                    public data class StringDeclaration(
                        override val name: String,
                        override val value: StringExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean = false,
                        override val synthetic: Boolean = false,
                        override val length: Int,
                        override val annotations: Map<String, List<Expression>> = emptyMap()
                    ) : Primitive

                    @Serializable
                    public sealed interface NumberDeclaration : Primitive {
                        public val isSigned: Boolean
                    }

                    @Serializable
                    public sealed interface IntDeclaration : NumberDeclaration {
                        override val value: IntExpression?

                        @Serializable
                        public data class Normal(
                            override val name: String,
                            override val value: IntExpression?,
                            override val nullable: Boolean = value == null,
                            override val mutable: Boolean,
                            override val private: Boolean,
                            override val comments: List<String> = emptyList(),
                            override val const: Boolean,
                            override val synthetic: Boolean = false,
                            override val length: Int,
                            override val isSigned: Boolean,
                            override val annotations: Map<String, List<Expression>> = emptyMap()
                        ) : IntDeclaration

                        @Serializable
                        public data class ReturnCode(
                            override val name: String,
                            override val value: IntExpression,
                            override val mutable: Boolean,
                            override val const: Boolean,
                            override val length: Int,
                            override val isSigned: Boolean,
                            override val annotations: Map<String, List<Expression>> = emptyMap()
                        ) : IntDeclaration {
                            override val private: Boolean = true
                            override val synthetic: Boolean = true
                            override val nullable: Boolean = false
                            override val comments: List<String> = emptyList()
                        }

                    }

                    @Serializable
                    public data class DoubleDeclaration(
                        override val name: String,
                        override val value: DoubleExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean,
                        override val synthetic: Boolean = false,
                        override val length: Int,
                        override val isSigned: Boolean,
                        override val annotations: Map<String, List<Expression>> = emptyMap()
                    ) : NumberDeclaration

                    @Serializable
                    public data class BooleanDeclaration(
                        override val name: String,
                        override val value: Expression.BooleanExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean,
                        override val synthetic: Boolean = false,
                        override val length: Int,
                        override val annotations: Map<String, List<Expression>> = emptyMap()
                    ) : Primitive

                    @Serializable
                    public data class ObjectDeclaration(
                        override val name: String,
                        val type: Type.Class,
                        val static: Boolean = false,
                        override val value: Expression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean = false,
                        override val private: Boolean = false,
                        override val comments: List<String> = emptyList(),
                        override val annotations: Map<String, List<Expression>> = emptyMap()
                    ) : Declaration
                }

                @Serializable
                public data class Assignment(
                    val declaration: Statement,
                    val newValue: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Math(
                    val declaration: Statement,
                    val value: Expression,
                    val op: Operation,
                    override val comments: List<String> = emptyList()
                ) : Statement {
                    @Serializable
                    public enum class Operation {
                        Add, Sub,
                        // Multi, Div
                    }
                }

                @Serializable
                public data class Static(
                    val type: Type.Class,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Use(
                    val target: Statement,
                    val action: Statement,
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression

                @Serializable
                public data class FunctionCall(
                    val function: Callable,
                    val parameters: List<Expression>,
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression

                @Serializable
                public data class Print(
                    val expr: StringExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Exit(
                    val returnVariable: IntExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Return(
                    val expr: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Throw(
                    val expr: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class TryCatch(
                    val tryStmts: List<Statement>,
                    val catchBlocks: List<CatchBlock>,
                    val finallyStmts: List<Statement>,
                    override val comments: List<String> = emptyList()
                ) : Statement {
                    @Serializable
                    public data class CatchBlock(
                        val exceptionClass: Type.Class,
                        val stmts: List<Statement>,
                        val comments: List<String> = emptyList(),
                    ) {
                        public val exception: Expression.ObjectVariable = Expression.ObjectVariable(
                            Declaration.ObjectDeclaration(
                                name = "exception",
                                type = exceptionClass,
                                static = false,
                                value = null,
                                nullable = false,
                            )
                        )
                    }
                }

                @Serializable
                public data class LoadExternal(val libName: String) : Statement {
                    override val comments: List<String> = emptyList()
                }

                @Serializable
                public data class DoWhile(
                    val statements: List<Statement>,
                    val condition: Expression.BooleanExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class While(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class For(
                    val counter: Declaration.NumberDeclaration,
                    val from: Expression.NumberExpression,
                    val step: Expression.NumberExpression? = null,
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement {
                    public constructor(
                        counter: Declaration.NumberDeclaration,
                        from: Expression.NumberExpression,
                        step: Expression.NumberExpression? = null,
                        condition: Expression.BooleanExpression,
                        comments: List<String> = emptyList(),
                        statements: Builder<Statement>.() -> Unit
                    ) : this(
                        counter, from, step, condition, build(statements), comments
                    )
                }

                @Serializable
                public data class ForEach(
                    val variable: Declaration,
                    val provider: Expression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class If(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement>,
                    val elseIfs: List<ElseIf> = emptyList(),
                    val elseStatements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression {
                    @Serializable
                    public data class ElseIf(
                        val condition: Expression.BooleanExpression,
                        val statements: List<Statement>,
                        val comments: List<String> = emptyList()
                    )
                }

                @Serializable
                public sealed interface When : Statement, Expression {
                    public val cases: List<Cases>
                    public val elseCase: Else?

                    @Serializable
                    public sealed interface Cases {
                        public val action: List<Statement>
                        public val comments: List<String>
                    }

                    @Serializable
                    public data class Single(
                        val expr: Expression,
                        override val cases: List<Case>,
                        override val elseCase: Else?,
                        override val comments: List<String> = emptyList()
                    ) : When {

                        @Serializable
                        public data class Case(
                            val condition: Expression,
                            override val action: List<Statement>,
                            override val comments: List<String> = emptyList()
                        ) : Cases
                    }

                    @Serializable
                    public data class Else(
                        val action: List<Statement>,
                        val comments: List<String> = emptyList()
                    )

                    @Serializable
                    public data class Multiple(
                        override val cases: List<Case>,
                        override val elseCase: Else?,
                        override val comments: List<String> = emptyList()
                    ) : When {
                        @Serializable
                        public data class Case(
                            val condition: Expression.BooleanExpression,
                            override val action: List<Statement>,
                            override val comments: List<String> = emptyList()
                        ) : Cases
                    }
                }
            }
        }

        @Serializable
        public sealed interface Type : Types {
            @Serializable
            public data class GlobalVariable(
                val declaration: Declaration,
                val doc: List<String>
            ) : Type, Expression.Variable {
                override val target: Declaration = declaration
            }

            @Serializable
            public data class Class(
                val name: String,
                val packageName: String,
                val constructor: List<Declaration> = emptyList(),
                val members: List<Declaration> = emptyList(),
                val functions: List<Function> = emptyList(),
                val doc: List<String> = emptyList(),
                val init: List<Function.Statement> = emptyList(),
                val isObject: Boolean = false,
                val isData: Boolean = false,
                val inner: List<Class> = emptyList(),
                val annotations: Map<String, List<String>> = emptyMap()
            ) : Type, Callable {

                public override fun declaration(): Class = copy(
                    members = emptyList(),
                    functions = emptyList(),
                    init = emptyList(),
                    inner = emptyList(),
                    doc = emptyList()
                )

                public fun variable(): Expression.ObjectVariable {
                    return Expression.ObjectVariable(
                        Declaration.ObjectDeclaration(
                            name = name,
                            comments = emptyList(),
                            type = this,
                            mutable = false,
                            private = false,
                            value = null
                        )
                    )
                }
            }

            @Serializable
            public sealed interface Natives : Type {
                @Serializable
                public data object Void : Natives

                @Serializable
                public data object Int : Natives

                @Serializable
                public data object String : Natives
            }
        }
    }

    @Serializable
    public sealed interface Expression {
        @Serializable
        public sealed interface Literal : Expression {
            public val value: Any
        }

        @Serializable
        public sealed interface Variable : Expression {
            public val target: Declaration
        }

        @Serializable
        public data class ObjectVariable(
            override val target: Declaration.ObjectDeclaration
        ) : Variable

        @Serializable
        public sealed interface StringExpression : Expression {
            @Serializable
            public data class StringLiteral(override val value: String) : StringExpression, Literal

            @Serializable
            public data class StringVariable(override val target: Declaration.StringDeclaration) :
                StringExpression, Variable {
                @Serializable
                public data class Use(
                    val target: ObjectVariable,
                    val variable: StringVariable,
                    override val comments: List<String> = emptyList()
                ) : Types.Function.Statement, StringExpression by variable
            }

            @Serializable
            public data class Concat(val left: Expression, val right: Expression) : StringExpression

            @Serializable
            public data class Interpolation(val expr: Expression) : StringExpression
        }

        @Serializable
        public sealed interface NumberExpression : Expression {

            @Serializable
            public sealed interface NumberVariable : Variable

            @Serializable
            public sealed interface IntExpression : NumberExpression {
                @Serializable
                public data class IntLiteral(override val value: Int) : IntExpression, Literal

                @Serializable
                public data class IntVariable(override val target: Declaration.IntDeclaration) :
                    IntExpression, NumberVariable {
                    @Serializable
                    public data class Use(
                        val target: ObjectVariable,
                        val variable: IntVariable,
                        override val comments: List<String> = emptyList()
                    ) : Types.Function.Statement, IntExpression by variable
                }
            }

            @Serializable
            public sealed interface DoubleExpression : NumberExpression {
                @Serializable
                public data class DoubleLiteral(override val value: Double) : DoubleExpression,
                    Literal

                @Serializable
                public data class DoubleVariable(override val target: Declaration.DoubleDeclaration) :
                    DoubleExpression, NumberVariable {
                    @Serializable
                    public data class Use(
                        val target: ObjectVariable,
                        val variable: DoubleVariable,
                        override val comments: List<String> = emptyList()
                    ) : Types.Function.Statement, DoubleExpression by variable
                }
            }
        }

        @Serializable
        public sealed interface BooleanExpression : Expression {
            @Serializable
            public data class BooleanLiteral(override val value: Boolean) : BooleanExpression,
                Literal

            @Serializable
            public data class Eq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            public data class NotEq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            public data class Not(val condition: BooleanExpression) : BooleanExpression

            @Serializable
            public data class Or(val left: BooleanExpression, val right: BooleanExpression) :
                BooleanExpression

            @Serializable
            public data class And(val left: BooleanExpression, val right: BooleanExpression) :
                BooleanExpression

            @Serializable
            public data class Bigger(
                val left: NumberExpression,
                val right: NumberExpression,
                val equals: Boolean = false
            ) : BooleanExpression

            @Serializable
            public data class Smaller(
                val left: NumberExpression,
                val right: NumberExpression,
                val equals: Boolean = false
            ) : BooleanExpression
        }
    }
}

public fun Declaration.variable(): Expression.Variable = when (this) {
    is Declaration.ObjectDeclaration -> Expression.ObjectVariable(
        this
    )

    is Declaration.BooleanDeclaration -> error("Not yet supported")
    is Declaration.DoubleDeclaration -> DoubleExpression.DoubleVariable(this)

    is Declaration.IntDeclaration -> IntExpression.IntVariable(this)

    is Declaration.StringDeclaration -> StringExpression.StringVariable(this)

    is Declaration.Array -> error("Not yet supported")
}
