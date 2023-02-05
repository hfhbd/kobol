package app.softwork.kobol.plugins.fir.renaming

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.*

public abstract class Rename(
    private val functions: String.() -> String,
    private val variables: String.() -> String,
    private val classes: String.() -> String
) : FirPluginBeforePhase {
    public companion object {
        public val next: Regex = "-(.)".toRegex()

        public fun String.toCamelCase(): String = lowercase().replace(next) {
            it.groups[1]!!.value.uppercase()
        }

        public fun String.toPascalCase(): String = toCamelCase().replaceFirstChar { it.uppercase() }

        public fun String.toSnakeCase(): String = lowercase().replace("-", "_").uppercase()
    }

    override fun invoke(tree: CobolFIRTree): CobolFIRTree {
        return tree.copy(
            env = tree.env?.let {
                it.copy(
                    inputOutput = it.inputOutput?.let {
                        it.copy(
                            fileControl = it.fileControl?.let {
                                it.copy(
                                    files = it.files.map {
                                        it.copy(
                                            fileStatus = it.fileStatus.variables()
                                        )
                                    }
                                )
                            }
                        )
                    }
                )
            },
            data = tree.data?.let {
                it.copy(
                    fileSection = it.fileSection.map {
                        it.rename()
                    },
                    workingStorage = it.workingStorage.rename(),
                    linkingSection = it.linkingSection.rename()
                )
            },
            procedure = tree.procedure.copy(
                topLevel = tree.procedure.topLevel.rename(),
                sections = tree.procedure.sections.map {
                    it.copy(
                        name = it.name.functions(),
                        statements = it.statements.rename()
                    )
                }
            )
        )
    }

    private fun CobolFIRTree.DataTree.File.rename() = copy(
        records = records.rename() as List<WorkingStorage.Record>,
        fileStatus = fileStatus.variables()
    )

    @JvmName("renameStatements")
    private fun List<CobolFIRTree.ProcedureTree.Statement>.rename(): List<CobolFIRTree.ProcedureTree.Statement> {
        return map {
            when (it) {
                is CobolFIRTree.ProcedureTree.Statement.Call -> it.copy(
                    name = it.name.functions(),
                    parameters = it.parameters.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Close -> it.copy(
                    file = it.file.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Continue -> it
                is CobolFIRTree.ProcedureTree.Statement.Display -> it.copy(
                    expr = it.expr.rename() as CobolFIRTree.ProcedureTree.Expression.StringExpression
                )

                is CobolFIRTree.ProcedureTree.Statement.Eval -> it.copy(
                    values = it.values.rename(),
                    conditions = it.conditions.map {
                        it.copy(
                            conditions = it.conditions.rename(),
                            action = it.action.rename()
                        )
                    },
                    other = it.other?.let {
                        it.copy(
                            action = it.action.rename()
                        )
                    }
                )

                is CobolFIRTree.ProcedureTree.Statement.ForEach -> it.copy(
                    variable = it.variable.rename() as WorkingStorage.Elementar.NumberElementar,
                    from = it.from.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression,
                    by = it.by?.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression?,
                    until = it.until.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression,
                    statements = it.statements.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.GoBack -> it
                is CobolFIRTree.ProcedureTree.Statement.If -> it.copy(
                    condition = it.condition.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression,
                    statements = it.statements.rename(),
                    elseStatements = it.elseStatements.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Move -> it.copy(
                    target = it.target.rename() as WorkingStorage.Elementar,
                    value = it.value.rename()
                )
                is CobolFIRTree.ProcedureTree.Statement.Add -> it.copy(
                    target = it.target.rename() as WorkingStorage.Elementar,
                    value = it.value.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Open -> it.copy(
                    file = it.file.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Perform -> it.copy(
                    sectionName = it.sectionName.functions(),
                    until = it.until?.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression?
                )

                is CobolFIRTree.ProcedureTree.Statement.Read -> it.copy(
                    file = it.file.rename(),
                    action = it.action.rename(),
                    atEnd = it.atEnd.rename()
                )

                is CobolFIRTree.ProcedureTree.Statement.Sql -> it.copy(
                    hostVariables = it.hostVariables.rename() as List<CobolFIRTree.ProcedureTree.Expression.Variable>,
                    parameter = it.parameter.rename() as List<CobolFIRTree.ProcedureTree.Expression.Variable>
                )

                is CobolFIRTree.ProcedureTree.Statement.While -> it.copy(
                    statements = it.statements.rename(),
                    until = it.until.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression
                )

                is CobolFIRTree.ProcedureTree.Statement.Write -> it.copy(
                    file = it.file.rename(),
                    from = it.from?.rename()
                )
            }
        }
    }

    @JvmName("renameExpression")
    private fun List<CobolFIRTree.ProcedureTree.Expression>.rename(): List<CobolFIRTree.ProcedureTree.Expression> {
        return map { it.rename() }
    }

    private fun CobolFIRTree.ProcedureTree.Expression.rename(): CobolFIRTree.ProcedureTree.Expression {
        return when (this) {
            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.And ->
                copy(
                    left = left.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression,
                    right = right.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression
                )

            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Equals -> copy(
                left = left.rename(),
                right = right.rename()
            )

            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Greater -> copy(
                left = left.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression,
                right = right.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression
            )

            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Not -> copy(
                target = target.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression,
            )

            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Or -> copy(
                left = left.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression,
                right = right.rename() as CobolFIRTree.ProcedureTree.Expression.BooleanExpression
            )

            is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Smaller -> copy(
                left = left.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression,
                right = right.rename() as CobolFIRTree.ProcedureTree.Expression.NumberExpression
            )

            is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral -> this
            is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> this
            is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable -> copy(
                target = target.rename() as WorkingStorage.Elementar.NumberElementar
            )

            is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> copy(
                left = left.rename(),
                right = right.rename()
            )

            is CobolFIRTree.ProcedureTree.Expression.StringExpression.Interpolation -> copy(
                value = value.rename()
            )

            is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> copy(
                target = target.rename() as WorkingStorage.Elementar.StringElementar
            )
        }
    }

    private fun List<WorkingStorage>.rename(): List<WorkingStorage> = map { it.rename() }

    private fun WorkingStorage.rename(): WorkingStorage = when (this) {
        is WorkingStorage.Elementar.EmptyElementar -> copy(
            name = name.variables(),
            recordName = recordName?.classes()
        )

        is WorkingStorage.Elementar.NumberElementar -> copy(
            name = name.variables(),
            recordName = recordName?.classes()
        )

        is WorkingStorage.Elementar.Pointer -> copy(
            name = name.variables(),
            recordName = recordName?.classes()
        )

        is WorkingStorage.Elementar.StringElementar -> copy(
            name = name.variables(),
            recordName = recordName?.classes()
        )

        is WorkingStorage.Record -> copy(
            name = name.classes(),
        )

        is WorkingStorage.Sql -> this
    }
}
