# Welcome to Erjang!  

Erjang is a virtual machine for Erlang, which runs on Java(tm).  

* For comments and questions please use the [Erjang Google Group](http://groups.google.com/group/erjang)
* Check the [[README]] before you try to run this.
* I am also posting updates at my blog, [Java Limit](http://javalimit.com)

### How does it work?

It loads Erlang's binary `.beam` file format, converts it into Java's `.class` file format, and loads it into the JVM.   It will eventually have it's own implementation of all Erlang's BIFs (built-in-functions) written in Java.  

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

<pre> krab$ ./ej 
Eshell V5.7.5  (abort with ^G)
1> erlang:display("hello world!").
"hello world!"
true
2> q().
krab$ 
</pre>

There are still things that doesn't work: a few BEAM instruction are missing some runtime support.  There are also BIFs missing, or only partially implemented; we're quite careful to throw @erjang.NotImplemented@ in BIFs (or branches thereof) which are not complete.  Many OTP modules need NIFs or linked-in drivers that are entirely missing or only partly implemented.

### Warnings

When you run Erjang, you're likely to get warnings like this:

<pre>Nov 10, 2010 5:15:56 PM erjang.EModuleManager$FunctionInfo$1 invoke
INFO: MISSING mnesia_sup:prep_stop/1</pre>

Such warnings are perfectly OK so long as you don't see a crash that you think is related to that.  It's a hint that perhaps there is a missing BIF somewhere around this.  But it may also just be some optional callback API which has not been implemented.

Until Erjang is a little more complete, I'd like to keep these warnings in there.


### What will it feel like to be running Erlang on the JVM?

Here is what to expect:

* In Erjang, every node runs on a single heap, and so global GC will sometimes happen.
* On the other hand, Erjang does not copy messages between processes -- they are simply shared, so sending large messages is significantly cheaper.
* Over all, you will loose the predictability in behavior that Erlang has with regard to GC pauses, because Erlang can GC each process individually.  Java GC continues to improve, so this might become less of an issue over time; but it will likely never go away.
* My current tests indicate, that you can get better throughput in Erjang than BEAM, see "this blog post":http://www.javalimit.com/2010/06/erjang-running-micro-benchmarks.html
* Erjang can run the "ring problem" at-par with BEAM, the Erlang virtual machine.  If you let the JIT warm up, Erjang looks like it is faster than beam.
* The big win is that Erjang is running on a VM that does dynamic compilation, selective inlining, and all the performance that comes from that.  


## Building

You should be able to do `ant jar`.  You need Perl version 5.10 or later, or you'll be unable to build the interpreter.

## Configuring

Adapt the file "erjang_cfg.properties" so that "erjang.otp.root"
points to the location of OTP (it is assumed to be an installed OTP
image, not just the source directory after running `make`; typical
values are "/usr/lib/erlang" and "/usr/local/lib/erlang").

## Running

<pre>renaissance:erjang krab$ ./ej
Eshell V5.7.5  (abort with ^G)
1> 3+4.
7
2> 
</pre>


When running, it writes files named `~/.erjang/${module}-${CRC}.jar`.  These
files are written in response to erlang:load_module(Module,Binary).

These files also serve as a cache of files translated from beam -> jar.
If something goes astray, it may help to remove the .erjang directory
forcing Erjang to recompile next time it runs.

### Prerequisites

I have only been testing this with Erlang/OTP R13.  ERJANG DOES NOT WORK WITH R14 since it introduced an incompatible API to the efile driver. 

If you run with a different erts (Erlang runtime system), then you can
use the +e <ErtsVsn> flag, like this:

   ./ej +e 5.7.5

to run with erts-5.7.5; alternatively, set the "erjang.erts.version"
property in the erjang_cfg.properties file.
Under normal circumstances, however, this should not be necessary;
Erjang should infer the correct version.



Cheers!

Kresten Krab Thorup
krab _at_ trifork dot com




