package com.github.prologdb.runtime.cliinterp

import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.output.MordantHelpFormatter
import java.nio.file.Paths

fun main(args: Array<String>) {
    ToplevelCommand(Paths.get("."))
        .context {
            helpFormatter = { ctx -> MordantHelpFormatter(ctx, showRequiredTag = true, showDefaultValues = true) }
        }
        .main(args)
}