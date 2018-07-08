package com.github.prologdb.runtime

/** Something that was constructed from actual prolog; thus possibly has source location information */
interface HasPrologSource {
    /** The source file name, null if unknown */
    val sourceFileName: String?

    /** The line number within [sourceFileName] at which the error occured, null if unknown */
    val sourceFileLine: Int?
}