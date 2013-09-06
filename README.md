<img src="https://raw.github.com/trifork/erjang/master/erjang_logo4.png" width="96px" align="right">

# Welcome to Erjang!  

[![Build Status](https://travis-ci.org/trifork/erjang.png)](https://travis-ci.org/trifork/erjang) 

Erjang is a virtual machine for Erlang, which runs on Java 7.  

* For comments and questions please use the [Erjang Google Group](http://groups.google.com/group/erjang)
* Check the [README](https://github.com/trifork/erjang/wiki/README) before you try to run this.
* I am also occasionally posting updates at my blog, [Java Limit](http://javalimit.com)

### Building...

````
> ant alljar
...

alljar:
      [jar] Building jar: erjang-R16B01.jar

BUILD SUCCESSFUL
Total time: 20 seconds
````
Then, just run `java -jar erjang-R16B01.jar`

### How does it work?

It loads Erlang's binary `.beam` file format, compiles it into Java's `.class` file format, and loads it into the JVM.   It will eventually have it's own implementation of all Erlang's BIFs (built-in-functions) written in Java.  

### Does it work?

Yes!  It does actually work.

- It can boot Erlang/OTP to the Eshell (`ej` command).
- There's a GUI console (`ejc` command) which supports ^G and line editing.  The console still needs some work [Swing wizards welcome here].
- Run Erlang distribution, tcp/ip, port commands (stdio to external processes).
- You can run the compiler (`c(foo)` command in the prompt)
- It runs `mnesia` with distribution across Erjang/BEAM nodes.
- The HTTP packet parsers are in the tcp/ip stack, so `mochiweb` and `webmachine` can run (without crypto for now).
- Larger systems like `rabbitmq` and `riak` can boot; and works for basic cases ... but it's not ready for prime time yet.
- Etc. etc.  Lot's of stuff work.

````erlang
> java -jar erjang-R16B01.jar
** Erjang R16B01 **  [root:/Users/krab/erlang/r16b01] [erts:5.10.2] [unicode]
Eshell V5.10.2  (abort with ^G)
1> 2+3.
5
2> 1/0.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 1 / 0
     in call from apply/3 
     in call from shell:apply_fun/3 (shell.erl, line 883)
     in call from erl_eval:do_apply/6 (erl_eval.erl, line 573)
     in call from shell:exprs/7 (shell.erl, line 674)
     in call from shell:eval_exprs/7 (shell.erl, line 629)
     in call from shell:eval_loop/3 (shell.erl, line 614)
     in call from apply/3 
3>
````

There are still things that doesn't work: There are BIFs missing, or only partially implemented; we're quite careful to throw `erjang.NotImplemented` in BIFs (or branches thereof) which are not complete.  Many OTP modules need NIFs or linked-in drivers that are entirely missing or only partly implemented.  We do have experimental support for NIFs, so that may be improving soon.



### What will it feel like to be running Erlang on the JVM?

Here is what to expect:

* In Erjang, every node runs on a single heap, and so global GC will sometimes happen.
* On the other hand, Erjang does not copy messages between processes -- they are simply shared, so sending large messages is significantly cheaper.
* Over all, you will loose the predictability in behavior that Erlang has with regard to GC pauses, because Erlang can GC each process individually.  Java GC continues to improve, so this might become less of an issue over time; but it will likely never go away.
* My current tests indicate, that you can get better throughput in Erjang than BEAM, see [this blog post](http://www.javalimit.com/2010/06/erjang-running-micro-benchmarks.html), the graphs from google charts broke.
* Erjang can run the "ring problem" at-par with BEAM, the Erlang virtual machine.  If you let the JIT warm up, Erjang looks like it is faster than beam.
* The big win is that Erjang is running on a VM that does dynamic compilation, selective inlining, and all the performance that comes from that.  


## Building

You should be able to do `ant jar`.  You need Perl version 5.10 or later, or you'll be unable to build the interpreter.

## Configuring

The only configuration you really need is to have an plain-old erlang installed, then Erjang will pick up the beam files using the `$PATH` to locate the `erl` binary, and then infer location of the beam files from there.  For instance when booting `ej`

````
./ej
** Erjang R16B01 **  [root:/Users/krab/erlang/r16b01] [erts:5.10.2] [unicode]
Eshell V5.10.2  (abort with ^G)
1> 
````

You can see that it picked up the root from `/Users/krab/erlang/r16b01`.  Alternatively you can pass an explicit `-root /some/path` to point erjang to a specific alternative erlang root.


## Running

When running, it writes files named `~/.erjang/${module}-${CRC}.jar`.  Each of these contain the JVM equivalent of an erlang module loaded into Erjang.

These files also serve as a cache of files translated from beam -> jar.
If something goes astray, it may help to remove the `~/.erjang` directory
forcing Erjang to recompile next time it runs.


Cheers!

Kresten Krab Thorup
krab _at_ trifork dot com




