package com.github.tmarsteel.ktprolog.parser.parser

private const val FEEDBACK_NOTE = "Please send the stacktrace and steps to reproduce to the author of this library."

class InternalParserError private constructor(actualMessage: Any) : Exception(actualMessage.toString()) {

    // the (Any) constructor serves a purpose:
    // Calling InternalParserError() should result in ex.message == FEEDBACK_NOTE
    // Calling InternalParserError(someMessage) should result in ex.message == someMessage + " " + FEEDBACK_NOTE
    // The any constructor is used to that (String) can be declared separately; otherwise the signatures would be
    // equal and the code would not compile.


    constructor() : this(FEEDBACK_NOTE as Any)
    constructor(message: String) : this((message + " " + FEEDBACK_NOTE) as Any)
}