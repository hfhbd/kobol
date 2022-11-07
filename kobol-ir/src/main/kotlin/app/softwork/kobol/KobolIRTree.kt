package app.softwork.kobol

import kotlinx.serialization.*
import kotlin.reflect.*

@Serializable
data class KobolIRTree(val name: String, val main: Types.Function, val types: List<Types>) {
    @Serializable
    sealed interface Types {
        @Serializable
        sealed interface Callable

        @Serializable
        data class Function(
            val name: String,
            val parameters: List<Statement.Declaration> = emptyList(),
            val returnType: Type,
            val body: List<Statement>,
            val private: Boolean = false,
            val doc: List<String> = emptyList(),
            val external: Boolean = false,
            val topLevel: Boolean = false,
            val packageName: String? = null
        ) : Types, Callable {

            fun declaration() = copy(body = emptyList())

            constructor(
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
            sealed interface Statement {
                val comments: List<String>

                @Serializable
                sealed interface Declaration : Statement {
                    val name: String
                    val mutable: Boolean
                    val private: Boolean
                    val value: Expression?
                    val nullable: Boolean
                    val annotations: Map<String, List<String>>

                    @Serializable
                    sealed interface Primitive : Declaration {
                        val const: Boolean
                        val length: Int
                    }

                    @Serializable
                    data class StringDeclaration(
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
                    sealed interface NumberDeclaration : Primitive

                    @Serializable
                    data class IntDeclaration(
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
                    data class DoubleDeclaration(
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
                    data class BooleanDeclaration(
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
                    data class ObjectDeclaration(
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
                data class Assignment(
                    val declaration: Statement,
                    val newValue: Expression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class Static(
                    val type: Type.Class,
                    override val comments: List<String> = emptyList()
                ): Statement

                @Serializable
                data class Use(
                    val target: Statement,
                    val action: Statement,
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression

                @Serializable
                data class FunctionCall(
                    val function: Callable,
                    val parameters: List<Expression>,
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression

                @Serializable
                data class Print(
                    val expr: Expression.StringExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class Exit(override val comments: List<String>) : Statement

                @Serializable
                data class LoadExternal(val libName: String) : Statement {
                    override val comments: List<String> = emptyList()
                }

                @Serializable
                data class DoWhile(
                    val functionCall: FunctionCall,
                    val condition: Expression.BooleanExpression,
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class While(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class For(
                    val counter: Declaration.NumberDeclaration,
                    val from: Expression.NumberExpression,
                    val step: Expression.NumberExpression? = null,
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class ForEach(
                    val variable: Declaration,
                    val provider: Expression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement

                @Serializable
                data class If(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement>,
                    val elseIfs: List<ElseIf> = emptyList(),
                    val elseStatements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ) : Statement, Expression {
                    @Serializable
                    data class ElseIf(
                        val condition: Expression.BooleanExpression,
                        val statements: List<Statement>,
                        val comments: List<String> = emptyList()
                    )
                }

                @Serializable
                sealed interface When : Statement, Expression {
                    val cases: List<Cases>
                    val elseCase: Else?

                    @Serializable
                    sealed interface Cases {
                        val action: List<Statement>
                        val comments: List<String>
                    }

                    @Serializable
                    data class Single(
                        val expr: Expression,
                        override val cases: List<Case>,
                        override val elseCase: Else?,
                        override val comments: List<String> = emptyList()
                    ) : When {

                        @Serializable
                        data class Case(
                            val condition: Expression,
                            override val action: List<Statement>,
                            override val comments: List<String> = emptyList()
                        ) : Cases
                    }

                    @Serializable
                    data class Else(
                        val action: List<Statement>,
                        val comments: List<String> = emptyList()
                    )

                    @Serializable
                    data class Multiple(
                        override val cases: List<Case>,
                        override val elseCase: Else?,
                        override val comments: List<String> = emptyList()
                    ) : When {
                        @Serializable
                        data class Case(
                            val condition: Expression.BooleanExpression,
                            override val action: List<Statement>,
                            override val comments: List<String> = emptyList()
                        ) : Cases
                    }
                }
            }
        }

        @Serializable
        sealed interface Type : Types {
            @Serializable
            data class GlobalVariable(
                val declaration: Function.Statement.Declaration,
                val doc: List<String>
            ) : Type, Expression.Variable {
                override val target: Function.Statement.Declaration = declaration
            }

            @Serializable
            data class Class(
                val name: String,
                val packageName: String,
                val constructor: List<Function.Statement.Declaration> = emptyList(),
                val members: List<Function.Statement.Declaration> = emptyList(),
                val functions: List<Function> = emptyList(),
                val doc: List<String> = emptyList(),
                val init: List<Function.Statement> = emptyList(),
                val isObject: Boolean = false,
                val isData: Boolean = false,
                val annotations: Map<String, List<String>> = emptyMap()
            ) : Type, Callable {

                fun variable(): Expression.ObjectVariable {
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
            object Void : Type
        }
    }

    @Serializable
    sealed interface Expression {
        @Serializable
        sealed interface Literal : Expression {
            val value: Any
        }

        @Serializable
        sealed interface Variable : Expression {
            val target: Types.Function.Statement.Declaration
        }

        @Serializable
        data class ObjectVariable(
            override val target: Types.Function.Statement.Declaration.ObjectDeclaration
        ) : Variable

        @Serializable
        sealed interface StringExpression : Expression {
            @Serializable
            data class StringLiteral(override val value: String) : StringExpression, Literal

            @Serializable
            data class StringVariable(override val target: Types.Function.Statement.Declaration.StringDeclaration) :
                StringExpression, Variable {
                @Serializable
                data class Use(
                    val target: ObjectVariable,
                    val variable: StringVariable,
                    override val comments: List<String> = emptyList()
                ) : Types.Function.Statement, StringExpression by variable
            }

            @Serializable
            data class Concat(val left: Expression, val right: Expression) : StringExpression

            @Serializable
            data class Interpolation(val expr: Expression) : StringExpression
        }

        @Serializable
        sealed interface NumberExpression : Expression {

            @Serializable
            sealed interface NumberVariable : Variable

            @Serializable
            sealed interface IntExpression : NumberExpression {
                @Serializable
                data class IntLiteral(override val value: Int) : IntExpression, Literal

                @Serializable
                data class IntVariable(override val target: Types.Function.Statement.Declaration.IntDeclaration) :
                    IntExpression, NumberVariable {
                    @Serializable
                    data class Use(
                        val target: ObjectVariable,
                        val variable: IntVariable,
                        override val comments: List<String> = emptyList()
                    ) : Types.Function.Statement, IntExpression by variable
                }
            }

            @Serializable
            sealed interface DoubleExpression : NumberExpression {
                @Serializable
                data class DoubleLiteral(override val value: Double) : DoubleExpression, Literal

                @Serializable
                data class DoubleVariable(override val target: Types.Function.Statement.Declaration.DoubleDeclaration) :
                    DoubleExpression, NumberVariable {
                    @Serializable
                    data class Use(
                        val target: ObjectVariable,
                        val variable: DoubleVariable,
                        override val comments: List<String> = emptyList()
                    ) : Types.Function.Statement, DoubleExpression by variable
                }
            }
        }

        @Serializable
        sealed interface BooleanExpression : Expression {
            @Serializable
            data class BooleanLiteral(override val value: Boolean) : BooleanExpression, Literal

            @Serializable
            data class Eq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            data class NotEq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            data class Not(val condition: BooleanExpression) : BooleanExpression

            @Serializable
            data class Or(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

            @Serializable
            data class And(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

            @Serializable
            data class Bigger(
                val left: NumberExpression,
                val right: NumberExpression,
                val equals: Boolean = false
            ) : BooleanExpression

            @Serializable
            data class Smaller(
                val left: NumberExpression,
                val right: NumberExpression,
                val equals: Boolean = false
            ) : BooleanExpression
        }
    }
}

fun KobolIRTree.Types.Function.Statement.Declaration.variable() = when (this) {
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

fun function(block: Builder<KobolIRTree.Types.Function.Statement>.() -> Unit): KobolIRTree.Types.Function =
    KobolIRTree.Types.Function(name = "fake", body = block)

operator fun KobolIRTree.Types.Function.getValue(receiver: Any?, prop: KProperty<*>): KobolIRTree.Types.Function =
    copy(name = prop.name)
