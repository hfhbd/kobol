package app.softwork.kobol.ir

import app.softwork.kobol.*
import kotlinx.serialization.*

/**
 * Never change the [id]
 */
@Serializable
public data class KobolIRTree(
    val id: String,
    val name: String,
    val main: Types.Function,
    val types: List<Types>
) {
    @Serializable
    public sealed interface Types {
        @Serializable
        public sealed interface Callable

        @Serializable
        public data class Function(
            val name: String,
            val parameters: List<Statement.Declaration> = emptyList(),
            val returnType: Type,
            val body: MutableList<Statement>,
            val private: Boolean = false,
            val doc: List<String> = emptyList(),
            val external: Boolean = false,
            val topLevel: Boolean = false,
            val packageName: String? = null,
            val inlineWith: Statement.FunctionCall? = null
        ) : Types, Callable {

            public fun declaration(): Function = copy(body = mutableListOf(), doc = emptyList())

            public constructor(
                name: String,
                parameters: List<Statement.Declaration> = emptyList(),
                returnType: Type = Type.Void,
                private: Boolean = false,
                doc: List<String> = emptyList(),
                external: Boolean = false,
                topLevel: Boolean = false,
                packageName: String? = null,
                body: List<Statement>
            ) : this(
                name,
                parameters,
                returnType,
                build(body),
                private,
                doc,
                external,
                topLevel,
                packageName
            )

            public constructor(
                name: String,
                parameters: List<Statement.Declaration> = emptyList(),
                returnType: Type = Type.Void,
                private: Boolean = false,
                doc: List<String> = emptyList(),
                external: Boolean = false,
                topLevel: Boolean = false,
                packageName: String? = null,
                body: Builder<Statement>.() -> Unit
            ) : this(
                name,
                parameters,
                returnType,
                build(body),
                private,
                doc,
                external,
                topLevel,
                packageName
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
                    public val annotations: Map<String, List<String>>

                    @Serializable
                    public sealed interface Primitive : Declaration {
                        public val const: Boolean
                        public val length: Int
                    }

                    @Serializable
                    public data class StringDeclaration(
                        override val name: String,
                        override val value: Expression.StringExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean = false,
                        override val length: Int,
                        override val annotations: Map<String, List<String>> = emptyMap()
                    ) : Primitive

                    @Serializable
                    public sealed interface NumberDeclaration : Primitive

                    @Serializable
                    public data class IntDeclaration(
                        override val name: String,
                        override val value: Expression.NumberExpression.IntExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean,
                        override val length: Int,
                        override val annotations: Map<String, List<String>> = emptyMap()
                    ) : NumberDeclaration

                    @Serializable
                    public data class DoubleDeclaration(
                        override val name: String,
                        override val value: Expression.NumberExpression.DoubleExpression?,
                        override val nullable: Boolean = value == null,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String> = emptyList(),
                        override val const: Boolean,
                        override val length: Int,
                        override val annotations: Map<String, List<String>> = emptyMap()
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
                        override val length: Int,
                        override val annotations: Map<String, List<String>> = emptyMap()
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
                        override val annotations: Map<String, List<String>> = emptyMap()
                    ) : Declaration
                }

                @Serializable
                public data class Assignment(
                    val declaration: Statement,
                    val newValue: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement
                
                @Serializable
                public data class Add(
                    val declaration: Statement,
                    val valueToAdd: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

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
                    val expr: Expression.StringExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Exit(
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class Return(
                    val expr: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                public data class LoadExternal(val libName: String) : Statement {
                    override val comments: List<String> = emptyList()
                }

                @Serializable
                public data class DoWhile(
                    val functionCall: FunctionCall,
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
                val declaration: Function.Statement.Declaration,
                val doc: List<String>
            ) : Type, Expression.Variable {
                override val target: Function.Statement.Declaration = declaration
            }

            @Serializable
            public data class Class(
                val name: String,
                val packageName: String,
                val constructor: List<Function.Statement.Declaration> = emptyList(),
                val members: List<Function.Statement.Declaration> = emptyList(),
                val functions: List<Function> = emptyList(),
                val doc: List<String> = emptyList(),
                val init: List<Function.Statement> = emptyList(),
                val isObject: Boolean = false,
                val isData: Boolean = false,
                val inner: List<Class> = emptyList(),
                val annotations: Map<String, List<String>> = emptyMap()
            ) : Type, Callable {

                public fun variable(): Expression.ObjectVariable {
                    return Expression.ObjectVariable(
                        Function.Statement.Declaration.ObjectDeclaration(
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
            public object Void : Type
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
            public val target: Types.Function.Statement.Declaration
        }

        @Serializable
        public data class ObjectVariable(
            override val target: Types.Function.Statement.Declaration.ObjectDeclaration
        ) : Variable

        @Serializable
        public sealed interface StringExpression : Expression {
            @Serializable
            public data class StringLiteral(override val value: String) : StringExpression, Literal

            @Serializable
            public data class StringVariable(override val target: Types.Function.Statement.Declaration.StringDeclaration) :
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
                public data class IntVariable(override val target: Types.Function.Statement.Declaration.IntDeclaration) :
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
                public data class DoubleLiteral(override val value: Double) : DoubleExpression, Literal

                @Serializable
                public data class DoubleVariable(override val target: Types.Function.Statement.Declaration.DoubleDeclaration) :
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
            public data class BooleanLiteral(override val value: Boolean) : BooleanExpression, Literal

            @Serializable
            public data class Eq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            public data class NotEq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            public data class Not(val condition: BooleanExpression) : BooleanExpression

            @Serializable
            public data class Or(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

            @Serializable
            public data class And(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

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

public fun KobolIRTree.Types.Function.Statement.Declaration.variable(): KobolIRTree.Expression.Variable = when (this) {
    is KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration -> KobolIRTree.Expression.ObjectVariable(this)
    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> error("Not yet supported")
    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable(
        this
    )

    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable(
        this
    )

    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> KobolIRTree.Expression.StringExpression.StringVariable(
        this
    )
}
