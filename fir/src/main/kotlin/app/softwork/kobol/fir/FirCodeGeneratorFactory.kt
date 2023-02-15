package app.softwork.kobol.fir

import java.io.*

public fun interface FirCodeGeneratorFactory {
    public operator fun invoke(outputFolder: File): FirCodeGenerator
}
