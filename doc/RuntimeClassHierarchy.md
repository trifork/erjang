# Run-time class hierarchy

## Introduction

A lot of the classes comprising a running Erjang system are generated
at runtime. The structures of these classes are not present directly in
the Erjang source code, but only indirectly, through the code that
generates them.
It can therefore be challenging to follow code involving such methods,
especially when there are more than a few levels of indirection.
The bytecode weaving is another confounder.

The structures of the run-time generated classes therefore merit some
documentation.  The following describes these classes in some detail.

## EFun and friends

Function objects are instances of classes which are generated at runtime.

The class hierarchy looks like this:

                  +---------+
                  | EObject |
                  +---------+
                       ^
                       |
                  +--------+
                  |  EFun  |
                  +--------+
        .............. ^ ....................... Classes below this line
                       |                         are generated at runtime
                       |
                 +-------------+
                 | EFun{arity} |
                 +-------------+
                       ^
                       |
               -------------------------------------------
               |                         |               |
        +---------------------+  +------------------+  +--------------------+
        | EFun{arity}Exported |  | EFun{arity}Guard |  | EFunHandler{arity} |
        +---------------------+  +------------------+  +--------------------+
               ^                         ^                   ^
               |                         |                   |
              ...                       ...                 ...

(`{arity}` stands for integers; there is thus an EFun0 and and EFun2Guard, etc.)

### Class `EFun`

    +-------------------------
    | EFun
    +-------------------------
    | ==== Invocation: ====
    | + EObject invoke(EProc proc, EObject[] args) // invoke-1
    | + EObject invoke(EProc proc, EObject[] args, int offset, int len) // invoke-2
    | + EObject apply(EProc proc, ESeq args)
    | + EObject go(EProc proc)
    | + EObject go2(EProc proc)
    |
    | ==== Introspection: ====
    | + abstract int arity()
    | + EObject info(EAtom aspect)
    | - EObject get_pid()
    | - ESeq get_env()
    | - FunID get_id()
    |   boolean is_local()
    +-------------------------

#### Invocation methods

Invocation of Erlang functions can happen in several ways:

- Static local calls - within a module;
- Static external calls - from module to module;
- Dynamic calls using (function objects/atoms) and a fixed-length parameter list (`F(A1,A2)`, `M:F(A1,A2)`);
- Dynamic calls using (function objects/atoms) and a variable-length parameter list (`erlang:apply(F,Args)`; `erlang:apply(M,F,Args)`).
- BIF calls (when called in interpreter mode).

Static local calls usually do not involve EFun objects.
Static external calls are handled (by the compile) through an
`EFun{arity}` object stored in a static field representing the remote function.


- `invoke-1` is the main entry point for the variable-length parameter list case.
- `invoke-2` is an adapter method used by the interpreter; it calls `invoke-1` with a slice of the given array.
- `go()` is used for implementing tail calls. The default implementation simply calls `go2()`.
- `go2()` is also used for implementing tail calls; it differs from `go()` in that `go2()` does not throw Pausable. The default implementation simply throws an exception.
  (TODO: Why not abstract?)
- `apply()` is an adapter method used for implementing `apply_last`?? and the `erlang:apply/*` BIFs.

#### Extension points:
Subclasses should
- implement `arity()`.
- implement `invoke-1`.
- override either `go()` or `go2()`, depending
  on whether the implementation might throw Pausable or not.


#### Introspection methods

- `arity()` is self-explanatory - it's the arity of the function.
- `info()`  and `is_local()` are used by the `erlang:fun_info/{1,2}` BIFs.
- `get_env()`, `get_id()`, `get_pid()` are in turn used by `info()`.

### Class `EFun{arity}`

`EFun{arity}` is the base class for all function objects of arity `{arity}`.

    +-------------------------
    | EFun{arity}
    +-------------------------
    | + @override int arity()
    | + @override EObject invoke(EProc proc, EObject[] args) // invoke-1
    | + abstract EObject invoke(EProc proc, EObject arg1, ..., EObject arg_arity) // invoke-3
    | - abstract EObject invoke_tail(EProc proc, EObject arg1, ..., EObject arg_arity)
    |
    | + static EFun{arity} cast(EObject obj)
    +-------------------------

- `invoke-1` is implemented as a (vararg) wrapper for `invoke-3`.
- `invoke-3` is the real invoker where the arity is known. It is implemented as a call to invoke_tail, plus a loop to implement tail recursion. Within the loop, `go()` is called as long as the tail-marker is returned.
- `invoke_tail` is at present not where the real work happens: it merely sets up a tail call and returns the tail-marker. This may change in the future.
- `go`/`go2` is the real worker; it is still abstract.

### Class `EFun{arity}Exported`

Represents exported Erlang functions.

((TODO))

### Class `EFun{arity}Guard`

Used by the interpreter for handling guard functions.  These are only
called by the interpreter, never used in tail calls, and is special in
that a null result means a null result (and must not, e.g., be
interpreted as a tail-call marker).

((TODO))

### Class `EFunHandler{arity}`

A bridge from EFun{arity} to the EFunHandler interface, as used by e.g. the interpreter, the Java interface, NIFs, and tuple-functions.

((TODO))

## ETuple and friends

((TODO))
