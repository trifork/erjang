Known functional deficiencies
=============================

(Note: This list is incomplete. Feel free to offer additions if you run into problems not described here.)

Scope limitations
-----------------

- Linked-in drivers are not supported. (And probably won't be.)

Missing BIFs
------------

-  Module `ets`
   - `match/3`, `match/1`
   - `rename/2`

Corner cases
------------

-  Module `ets`
   - `setopts/2`: Atomicity: entire option list is not validated before changes are applied

- Possible race condition between process status updates
  _(SOLVED in process-lifecycle-consistency branch)_
- Demonstrated race condition between process status=SIG_EXIT and exists()
  _(SOLVED in process-lifecycle-consistency branch)_
- Exit hook vs. process state race
  _(SOLVED in process-lifecycle-consistency branch)_

"Method too large" limitations
------------------------------

Some Erlang functions are (at present) too bit and complex for
translation into Java methods; this is due to the 64KB code size
limitation for Java methods.

For the present, Erjang will fall back to interpretation in these cases - for _the entire module_, which may of course affect performance.

Functions known to be problems are:

- `hipe` application:
  - Some HiPE functions.
- `yaws` application:
  - `mime_types:t/2`

Tuples of size >= 256
---------------------

Because of Java limitations hit by our present code generator strategy,
tuples of size >= 256 won't work.
See https://github.com/trifork/erjang/issues/78 .
