/**
 * With coroutines, the unit of concurrency is no longer [Thread] but the root corotuine (if others are spawned
 * below). The help identify these coroutines have a [principal]; in prologdb, this would be the query.
 */
package com.github.prologdb.async

import java.util.*

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
class PrincipalConflictException(val principalInError: Principal, val violatedPrincipal: Principal):
    RuntimeException("Code of principal $principalInError attempted to execute code belonging to principal $violatedPrincipal")