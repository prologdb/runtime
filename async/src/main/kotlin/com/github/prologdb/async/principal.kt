/**
 * With coroutines, the unit of concurrency is no longer [java.lang.Thread] but the root corotuine (if others are spawned
 * below). The help identify these coroutines have a [Principal]; in prologdb, this would be the query.
 */
package com.github.prologdb.async

import java.util.UUID

typealias Principal = Any

/**
 * To be used as the concurrency principal for [WorkableFuture] and [LazySequence]
 * if they have exclusive access to their state (e.g. [LazySequence.of])
 */
object IrrelevantPrincipal : Principal()

val RANDOM_PRINCIPAL: Principal
    get() = UUID.randomUUID()

/**
 * Thrown when coroutine code belonging to [principalInError] attempts to invoke coroutine code
 * belonging to [violatedPrincipal]. This attempt is comparable to one [Thread] attempting to change
 * thread-local (e.g. stack) variables of another thread.
 */
class PrincipalConflictException private constructor(val principalInError: Principal, val violatedPrincipal: Principal):
    RuntimeException("Code of principal $principalInError attempted to execute code belonging to principal $violatedPrincipal") {
    companion object {
        fun requireCompatible(current: Principal, newlyJoined: Principal) {
            if (current === IrrelevantPrincipal || newlyJoined === IrrelevantPrincipal) {
                return
            }

            if (current != newlyJoined) {
                throw PrincipalConflictException(current, newlyJoined)
            }
        }
    }
}