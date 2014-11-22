NieA
====

A pure subset of scheme
- compililation to C
- reference counting for memory management


Mathematical Justification for ref-counting lambda
==================================================

Consider a counter "t" that increments every time an allocation is performed.

The language is pure and strict, so a vector allocation at time t will only ever be able to put objects from times <t into it.

This means there is no cycles so ref-counting be able to reclaim all memory.



Implementation notes
====================

It would probably suffice to consider these two examples:

```
(define (k x y)
  x)

(define (s x y z)
  ((x z)
   (y z)))
```

Calling convention:

CALLER:
when you pass something into a function you are essentially handing over your reference to it.
If you want to continue to use that object afterwards you will need to inc it, otherwise just pass it in and forget.

CALLEE:
You have one reference to each parameter passed in, you must dec them before returning.
You're handing over a reference to the caller when you return something.

```
(define (k x y)
  (set! result x)
  (dec y)
  (return result))

(define (s x y z)
  (inc z)
  (set! result ((x z)
                (y z)))
  (return result))
```
