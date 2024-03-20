package com.github.prologdb.runtime.cliinterp

import java.nio.file.Paths

fun main(args: Array<String>) {
    PrologDbInterpreterCommand(Paths.get(".")).main(args)
}