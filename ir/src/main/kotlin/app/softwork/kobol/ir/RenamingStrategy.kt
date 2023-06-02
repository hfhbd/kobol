package app.softwork.kobol.ir

public interface RenamingStrategy {
    public fun variable(variable: KobolIRTree.Expression.Variable): String
    public fun function(function: KobolIRTree.Types.Function): String
    public fun klass(klass: KobolIRTree.Types.Type.Class): String
}
