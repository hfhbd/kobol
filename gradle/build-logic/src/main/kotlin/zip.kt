import org.gradle.api.provider.Provider

internal fun <L, C, R, V> zip(
    left: Provider<L>,
    center: Provider<C>,
    right: Provider<R>,
    combiner: (L, C, R) -> V,
) = left.zip(center) { l, c -> l to c }.zip(right) { (l, c), r -> combiner(l, c, r) }

internal fun <L, C, D, R, V> zip(
    left: Provider<L>,
    center: Provider<C>,
    center2: Provider<D>,
    right: Provider<R>,
    combiner: (L, C, D, R) -> V,
) = left.zip(center) { l, c -> l to c }
    .zip(center2) { lc, d ->
        lc to d
    }
    .zip(right) { (lc, d), r ->
        combiner(lc.first, lc.second, d, r)
    }

internal fun <L, C, D, E, R, V> zip(
    left: Provider<L>,
    center: Provider<C>,
    center2: Provider<D>,
    center3: Provider<E>,
    right: Provider<R>,
    combiner: (L, C, D, E, R) -> V,
) = left.zip(center) { l, c -> l to c }
    .zip(center2) { lc, d ->
        lc to d
    }.zip(center3) { lcd, e ->
        lcd to e
    }
    .zip(right) { (lcd, e), r ->
        val (lc, d) = lcd
        combiner(lc.first, lc.second, d, e, r)
    }
