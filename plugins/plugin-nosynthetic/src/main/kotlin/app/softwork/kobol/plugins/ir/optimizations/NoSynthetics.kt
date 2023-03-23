package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.IrPlugin
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
public class NoSynthetics : AbstractInlining(true)
