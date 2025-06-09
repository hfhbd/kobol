import org.gradle.api.provider.Provider

internal fun <L : Any, C : Any, R : Any, V: Any> zip(
    left: Provider<L>,
    center: Provider<C>,
    right: Provider<R>,
    combiner: (L, C, R) -> V,
) = left.zip(center) { l, c -> l to c }.zip(right) { (l, c), r -> combiner(l, c, r) }
