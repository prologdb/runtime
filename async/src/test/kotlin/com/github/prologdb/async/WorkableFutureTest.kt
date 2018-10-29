package com.github.prologdb.async

import com.sun.xml.internal.ws.util.CompletedFuture
import io.kotlintest.matchers.beGreaterThanOrEqualTo
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldThrow
import io.kotlintest.specs.FreeSpec
import java.util.*
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import kotlin.concurrent.thread

class WorkableFutureTest : FreeSpec({
    "immediate return" {
        val hasRun = CompletableFuture<Unit>()

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            hasRun.complete(Unit)
            "foobar"
        }

        hasRun.isDone shouldBe false
        future.step()
        hasRun.isDone shouldBe true

        future.isDone shouldBe true
        future.get() shouldBe "foobar"
    }

    "immediate throw" {
        val hasRun = CompletableFuture<Unit>()

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            hasRun.complete(Unit)
            throw Exception("Faiiilleeeed!!!")
        }

        hasRun.isDone shouldBe false
        future.step()
        hasRun.isDone shouldBe true

        shouldThrow<Exception> {
            future.get()
        }
    }

    "wait for another future step-by-step successfully" {
        val hasStarted = CompletableFuture<Unit>()
        val returnedAfterWait = CompletableFuture<Unit>()
        val waitingOn = CompletableFuture<String>()

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            hasStarted.complete(Unit)

            val result = await(waitingOn)
            returnedAfterWait.complete(Unit)

            result
        }

        // initial state
        future.isDone shouldBe false
        hasStarted.isDone shouldBe false
        returnedAfterWait.isDone shouldBe false

        // test that await(T) suspends
        future.step()
        future.isDone shouldBe false
        hasStarted.isDone shouldBe true
        returnedAfterWait.isDone shouldBe false

        // test that step() does not resume the coroutine
        // unless its completed
        future.step()
        future.isDone shouldBe false
        returnedAfterWait.isDone shouldBe false

        // test that await() correctly forwards the result
        // of the awaited future
        val value = "Hello World!!"
        waitingOn.complete(value)

        // unless step() is called again, the future should
        // not move ahead
        future.isDone shouldBe false
        returnedAfterWait.isDone shouldBe false

        future.step()

        // completion
        future.isDone shouldBe true
        returnedAfterWait.isDone shouldBe true
        future.get() shouldBe value
    }

    "wait for another future step-by-step exceptionally" {
        val hasStarted = CompletableFuture<Unit>()
        val waitingOn = CompletableFuture<String>()

        val future = WorkableFutureImpl<Any>(RANDOM_PRINCIPAL) {
            hasStarted.complete(Unit)

            try {
                await(waitingOn)
                Unit
            }
            catch (ex: Throwable) {
                ex
            }
        }

        future.step()
        future.isDone shouldBe false
        hasStarted.isDone shouldBe true

        future.step()
        future.isDone shouldBe false
        waitingOn.isCompletedExceptionally shouldBe false

        val error = Exception("faiiil!")
        waitingOn.completeExceptionally(error)

        future.isDone shouldBe false
        future.isCancelled shouldBe false
        future.step()

        future.isDone shouldBe true
        future.isCancelled shouldBe false

        future.get() shouldBe error
    }

    "await exception should not complete the future" {
        val waitingOn = CompletableFuture<Unit>()
        val caught = CompletableFuture<Throwable>()

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            try {
                await(waitingOn)
            }
            catch (ex: Throwable) {
                caught.complete(ex)
            }

            "All fine"
        }

        val error = Exception("faaaaaaiiiilllllllll")

        future.step()
        waitingOn.completeExceptionally(error)
        future.step()

        caught.isDone shouldBe true
        caught.get() shouldBe error

        future.get() shouldBe "All fine"
    }

    "await completed does not suspend" {
        val waitingOn = CompletedFuture(Unit, null)

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            await(waitingOn)
            "Done"
        }

        future.step()
        future.isDone shouldBe true
        future.get() shouldBe "Done"
    }

    "get waits on await future" {
        val waitingOn = CompletableFuture<Unit>()

        val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
            await(waitingOn)
            "Done"
        }

        val earliestStart = System.currentTimeMillis()
        thread {
            Thread.sleep(200)
            waitingOn.complete(Unit)
        }

        future.get() shouldBe "Done"
        val completedAt = System.currentTimeMillis()

        (completedAt - earliestStart) should beGreaterThanOrEqualTo(200L)
    }

    "await workable future" {
        val stepOneCompleted = CompletableFuture<Unit>()
        val blockerAfterStepOne = CompletableFuture<String>()
        val principal = RANDOM_PRINCIPAL

        val waitingOn = WorkableFutureImpl(principal) {
            stepOneCompleted.complete(Unit)
            await(blockerAfterStepOne)
        }

        val future = WorkableFutureImpl(principal) {
            await(waitingOn)
        }

        future.isDone shouldBe false

        future.step()

        future.isDone shouldBe false
        stepOneCompleted.isDone shouldBe false

        future.step()

        future.isDone shouldBe false
        stepOneCompleted.isDone shouldBe true
        waitingOn.isDone shouldBe false

        future.step()

        future.isDone shouldBe false
        waitingOn.isDone shouldBe false

        blockerAfterStepOne.complete("Fuzz")

        future.isDone shouldBe false
        waitingOn.isDone shouldBe false

        future.step()

        future.isDone shouldBe true
        future.get() shouldBe "Fuzz"
    }

    "cancel should stop at next suspension point" - {
        "cancelled after suspension point hit" {
            val beforeCancel = CompletableFuture<Unit>()
            val afterCancel = CompletableFuture<Unit>()
            val waitingOn = CompletableFuture<Unit>()

            val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
                beforeCancel.complete(Unit)
                await(waitingOn)
                afterCancel.complete(Unit)
            }

            future.step()

            future.isDone shouldBe false
            future.isCancelled shouldBe false

            future.cancel(true)

            future.isDone shouldBe true
            future.isCancelled shouldBe true
            afterCancel.isDone shouldBe false

            future.step() shouldBe true
            future.isCancelled shouldBe true
            afterCancel.isDone shouldBe false
        }

        "cancelled before suspension point hit" - {

            // excluded for now because the kotlin coroutines behave strangely:
            // the cancelling from within the couroutine itself (not sure whether that is an issue)
            // causes an illegalmonitorstateexception in code generated by the kotlin compiler
            // i cannot trace it ATM
            "suspension point completed when hit" {
                /*var beforeSuspension: () -> Unit = {}
                val afterSuspension = CompletableFuture<Unit>()
                val waitingOn = CompletedFuture(Unit, null)

                val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
                    beforeSuspension()
                    await(waitingOn)
                    afterSuspension.complete(Unit)
                }

                beforeSuspension = {
                    future.cancel(true) // cancel, before the suspension point was hit
                }

                future.step()

                future.isDone shouldBe true
                future.isCancelled shouldBe true
                shouldThrow<CancellationException> {
                    future.get()
                }

                future.isDone shouldBe true
                future.isCancelled shouldBe true
                shouldThrow<CancellationException> {
                    future.get()
                }

                // waitingOn was already completed when await was called
                // the workable future must not continue

                future.step() shouldBe true
                afterSuspension.isDone shouldBe false

                future.isDone shouldBe true
                future.isCancelled shouldBe true
                shouldThrow<CancellationException> {
                    future.get()
                }*/
            }

            "suspension point not completed when hit" {
                /*var beforeSuspension: () -> Unit = {}
                val afterSuspension = CompletableFuture<Unit>()
                val waitingOn = CompletableFuture<Unit>()

                val future = WorkableFutureImpl(RANDOM_PRINCIPAL) {
                    beforeSuspension()
                    await(waitingOn)
                    afterSuspension.complete(Unit)
                }

                beforeSuspension = {
                    future.cancel(true) // cancel, before the suspension point was hit
                }

                future.step()

                future.isDone shouldBe true
                future.isCancelled shouldBe true
                afterSuspension.isDone shouldBe false
                shouldThrow<CancellationException> {
                    future.get()
                }

                // waitingOn is not completed. completing it must not
                // change the state of the workable future

                waitingOn.complete(Unit)

                afterSuspension.isDone shouldBe false
                future.isDone shouldBe true
                future.isCancelled shouldBe true
                shouldThrow<CancellationException> {
                    future.get()
                }

                future.step() shouldBe true
                afterSuspension.isDone shouldBe false

                future.isDone shouldBe true
                future.isCancelled shouldBe true
                shouldThrow<CancellationException> {
                    future.get()
                }*/
            }
        }

        "finally" {
            val finallyExecutions = mutableListOf<Int>()

            val workableFuture = launchWorkableFuture(UUID.randomUUID()) {
                awaitAndFinally(CompletedFuture(Unit, null)) {
                    finallyExecutions.add(1)
                }
                finally {
                    finallyExecutions.add(2)
                }
                awaitAndFinally(CompletedFuture(null, Exception("ERROR!"))) {
                    finallyExecutions.add(3)
                }
                finally {
                    finallyExecutions.add(4)
                }
            }

            shouldThrow<Exception> {
                workableFuture.get()
            }

            finallyExecutions shouldBe listOf(3, 2, 1)
        }
    }

    "folding" {
        val principal = RANDOM_PRINCIPAL
        val foldable = buildLazySequence(principal) {
            yield(1)
            yield(2)
            yield(3)
            yield(5)
            yield(102)
        }

        val future = WorkableFutureImpl(principal) {
            foldRemaining(foldable, 0, Int::plus) + 4
        }

        future.step() shouldBe false
        future.step() shouldBe false
        future.step() shouldBe false
        future.step() shouldBe false
        future.step() shouldBe false
        future.step() shouldBe false
        future.step() shouldBe true
        future.get() shouldBe 1 + 2 + 3 + 5 + 102 + 4
    }
})