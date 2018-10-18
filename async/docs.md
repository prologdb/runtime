# Package com.github.prologdb.async

Facilities to model and simplify asynchronous processing. The Sequence classes provided
by Kotlin are not suitable for application in prolog search trees (explained below).

## Motivation

Java provides `Future<T>`. For simple operations (e.g. [java.nio.channels.AsynchronousFileChannel.read]),
these are atomic and splitting them up even more is not meaningfully possible (control
is deferred to the OS, beyond the abstraction layer of the JVM or Kotlin). However, in prolog,
very complex operations take place that are composed of a lot of such small operations.
This package exists to make these asynchronous aiming to optimize resource usage when a lot
of prolog search trees are executed in parallel.

### Why not kotlin.sequences.Sequence<T>?

The interface `Sequence<T>` has only one method: it exposes an `Iterator<T>`, so lets look
at that.  
`Iterator<T>` has the method `hasNext(): Boolean` and this does not fit with prolog
solution search: As long as there are choice-points left, it is not known whether there are
any more solutions. Calling `hasNext()` is **forced** to calculate the next solution so that
it can return according to the contract. This means that `hasNext()` will block the thread for
a significant amount of time when faced with non-trivial searches. This behaviour does not work
well in consumer code, especially the kotlin built-in for-loop (as opposed to the
`forEachRemaining` method).

Declaring a new interface forces consumer code to take that into consideration. Also,
as you will see with `WorkableLazySequence`, a custom coroutine builder was not avoidable. 

---

The classes in this package are generally not thread-safe. Most of them have an internal
mutex that will block multiple threads from performing work and can easily lead to deadlocks.
However, it is safe to have multiple threads call methods on the same object as long as
they don't do so simultaneously. An external locking mechanism is **strongly advised**.

## LazySequence

Like `Future<T>`, but with an (arbitrarily) sorted list of results (instead of only one).
Is designed for work that is entirely CPU or memory bound.

From a resource-efficiency viewpoint, there is no reason to split CPU/memory bound tasks. Prolog,
in its design, already has a pretty clear split: one search tree can yield multiple solutions
where each solution is calculated individually, step-by-step.  
The idea behind LazySequence is to allow the caller to control when the solutions are calculated.
As in prolog shells, the calculation is only attempted when more results are requested.
Upon request, only the next solution is calculated and the process is put on hold until the
next request comes in.

### Consuming a LazySequence

This exact same semantic is mapped by the method [LazySequence.tryAdvance]: it attempts to
calculate the next solution. If it finds one, the solution is returned. If there are no more
solutions left, it returns null (the search is depleted).  
Calling `tryAdvance()` is exactly the same thing as typing `;` into a prolog prompt during
a search.

Further, these sequences can be closed before being depleted. If
a user decides that they don't need any further solutions, they
can quit the search without having used any resources on the
solutions they didn't actually use. This is modeled by
the [LazySequence.close] method. It also allows the system to release
memory & disk space associated with the search.  
Calling `close()` is exactly the same thing as typing `.` into a prolog prompt during
a search.

There are a couple of member methods defined on [LazySequence] that are implemented in terms
of calling tryAdvance() and sometimes discarding the result (like `skip(Int)`) but could also
be implemented more efficiently, depending on implementation details.

Other methods known from working with the usual Kotlin sequences are availble as extension methods,
e.g. `map` or `maxBy`.

### Providing a LazySequence

Use the builtin helper function `buildLazySequence`:

```kotlin
val sequence = buildLazySequence {
    yield("Foo")
    yield("Bar")
    yieldAll(buildLazySequence {
        yield("Fizz")
        yield("Buzz")
    })
}

assert(sequence.tryAdvance() == "Foo")
assert(sequence.tryAdvance() == "Bar")
assert(sequence.tryAdvance() == "Fizz")
assert(sequence.tryAdvance() == "Buzz")
assert(sequence.tryAdvance() == null)

// assuming the sequence would start over
// you could also do things like that:
val el = sequence
    .skip(1)
    .map { it + "!!" }
    .minBy { it.length }
    
assert(el == "Bar!!")
```

### Caveats

When using functional methods that work on elements of the list (such as map,
transformExceptionsOnRemaining), you end up with two instances of LazySequence. **Both
objects consume from the same source!** To illustrate:

```kotlin
val sequence = buildLazySequence {
    yield("A")
    yield("BB")
    yield("CCC")
}
val mapped = sequence.map { it.length }

assert(sequence.tryAdvance() == "A")
assert(mapped.tryAdvance() == 2) // mapped from "BB"
assert(sequence.tryAdvance() == "CCC")
```

## WorkableLazySequence

Things get more complicated when disk or network I/O is necessary in order to compute a
solution. As you may have guessed, `tryAdvance()` is blocking: it either does all working
(and wasteful waiting!) to get to the next solutions or it declares the solutions depleted.
Blocking on I/O is bad for efficiency and is exactly what this module tries to avoid.

### Consuming a WorkableLazySequence

WorkableLazySequence to the rescue: it provides the additional method `step(): State`:  
When called, it attempts to do the next package of work. If there is useful work to be
done, the calculation will continue until it comes to a point where the process has to wait
for an external resource.
If that work results in a solution being available `step()` will return `State.RESULTS_AVAILABLE`.
In that case it is guaranteed that the next call to `tryAdvance()` will return a solution immediately
without doing any expensive work.  
If the work done in the `step()` invocation did not yield a result yet, `step()` will return
`State.PENDING`.  
When `step()` is called while the resource the process is waiting for has not yet returned its
result the return value of `step()` will also be `PENDING`. 

When there are many `WorkableLazySequence`s to work on resource usage can be made more efficient by
having (one or multiple) worker threads call `step()` on each of them in a round-robin fashion.
As soon as any of the sequences return `RESULTS_AVAILABLE`, the solution can be published to the
interested party.  
This pattern turns the blocking calls to `tryAdvance()` into an event publisher that uses the
available resources very efficiently.

#### Example with worker thread

```kotlin
val results = LinkedBlockingQueue<String>()
val allDone = CompletableFuture<Unit>()

// start the worker
thread {
    val sequences: List<WorkableLazySequence<String>>
    while (sequences.isNotEmpty()) {
        val iterator = sequences.iterator()
        while (iterator.hasNext()) {
            val sequence = iterator.next()
            var state = sequence.step()
            if (state == WorkableLazySequence.State.RESULTS_AVAILABLE) {
                results.add(sequence.tryAdvance())
                state = sequence.state 
            }
            if (state == WorkableLazySequence.State.DEPLETED) {
                iterator.remove()
            }
        }
    }
    
    allDone.complete(Unit)
}

// consume in another thread, using resources only when results are there
while (!allDone.isDone) {
    val result = results.poll(1, TimeUnit.SECONDS)
    if (result != null) {
        // handle
    }
}

```

### Caveats

#### Behaviour of `step()`

One invocation to `step()` could yield multiple solutions at once. In that case, the
WorkableLazySequence is allowed to cache these solutions. If that is done, `step()` will
return `RESULTS_AVAILABLE`. If you then go ahead and consume one solution by calling
`tryAdvance()` these additional solutions will not be lost.    
Suppose the sequence is now waiting for an I/O but there are more solutions cached from the
previous call to `step()` that yielded multiple solutions. If you now call `step()`, it will
determine that there is no useful work to do. But instead of returning `PENDING`, it will
return `RESULTS_AVAILABLE` as long as there are cached solutions.  
This means that you cannot infer from the return type of `step()` whether the call did actual
work on the sequence.

#### Behaviour of `tryAdvance()`

As per the contract of LazySequence, `tryAdvance()` **must** either find the next solution
or, once and for all, declare the sequence as finished. Thus, if you call `tryAdvance()` on
a WorkableLazySequence, it will repeatedly call `step()` until a solution is available (
optimizations like `Future.join()` included not to waste CPU in a `while(true)`)

In cause you just need to check whether there are solutions and want to avoid possibility spending
precious time doing work on the sequence, use the [WorkableLazySequence.state] property. If that
returns `RESULTS_AVAILABLE`, you are guaranteed that `tryAdvance()` will immediately return a
non-null value.

## WorkableFuture

Implements `Future<T>` and adds the `step()` method with the same semantics as in
`WorkableLazySequence`. Calling `get()` on a workable future will call `step()` repeatedly
until done.

```kotlin
fun insertRecord(val id: Long, data: ByteArray): WorkableFuture<Unit> {
    val existingRecord: WorkableFuture<ByteArray?> = readRecord(id)
    awaitAndThenWorkable(existingRecord) { data: ByteArray? ->
        if (data == null) {
            // record does not exist => write
            await(syncFileChannel.write(data, targetOffset))
        } else {
            throw IOException("Record with id $id already exists")
        }
    }
}

val future = insertRecord(121, ByteArray(20))

assert(future.step() == false)
// goes until awaitAndThenWorkable and then waits for the disk read
// let some time pass, disk read completed

assert(future.step() == false)
// goes until await() and then waits for the disk write
// let some time pass, disk write completed

assert(future.isDone == false)

assert(future.step() == true)
// resumes from await() and completes the future

assert(future.isDone == true)
future.get() // returns Unit immediately 

// ------

// attempt to insert duplicate
val future = insertRecord(121, ByteArray(21))

assert(future.step() == false)
// goes until awaitAndThenWorkable and then waits for the disk read
// let some time pass, disk read completed

assert(future.step() == true)
// sees that the record exists and throws exception; is not forwarded by step()

assert(future.isDone)
assertThrows<IOException>() {
    future.get()
}
```

[java.nio.channels.AsynchronousFileChannel.read]: https://docs.oracle.com/javase/10/docs/api/java/nio/channels/AsynchronousFileChannel.html#read(java.nio.ByteBuffer,long)