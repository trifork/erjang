==== How to build using CMake ====

To build JNIF, run:

    cmake . && make

This should result in a shared library "jnif" to be generated.
The library resides in a file named as conventional on the platform,
i.e. "libjnif.so" on Unix, "libjnif.dylib" on Darwin, etc.

The build script has been tested on:

- Linux (Ubuntu), CMake 2.8.7
