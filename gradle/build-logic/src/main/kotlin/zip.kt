import org.gradle.api.provider.Provider

internal fun <L, C, R, V> zip(
    left: Provider<L>,
    center: Provider<C>,
    right: Provider<R>,
    combiner: (L, C, R) -> V,
) = left.zip(center) { l, c -> l to c }.zip(right) { (l, c), r -> combiner(l, c, r) }
