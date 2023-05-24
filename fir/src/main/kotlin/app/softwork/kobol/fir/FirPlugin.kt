package app.softwork.kobol.fir

public sealed interface FirPlugin : AutoCloseable {
    override fun close() {}
}

/**
 * Called right after generating a single tree
 */
public fun interface FirPluginBeforePhase : FirPlugin {
    public operator fun invoke(tree: CobolFIRTree): CobolFIRTree
}

/**
 * Called after generating all trees
 */
public fun interface FirPluginAfterPhase : FirPlugin {

    /**
     * @param tree The current main tree and other trees
     */
    public operator fun invoke(tree: CobolFIRTree, other: Iterable<CobolFIRTree>): Iterable<CobolFIRTree>
}
