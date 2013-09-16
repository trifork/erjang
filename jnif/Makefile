

JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
JAVA_ARCH=darwin
ERTS_HOME=/Users/krab/Projects/erjang/x86_64-apple-darwin12.4.0/erts-5.10.2

ERJANG_JAR=/Users/krab/Projects/erjang/erjang-0.2.jar

CXXFLAGS=-I${JAVA_HOME}/include \
	-I${JAVA_HOME}/include/${JAVA_ARCH} \
	-I${ERTS_HOME}/include \
	-g

OFILES= jnif.o \
	jnif_env.o \
	jnif_binary.o \
	jnif_resource.o \
	jnif_lock.o \
	jnif_number.o \
	jnif_string.o \
	jnif_atom.o \
	jnif_tuple.o \
	jnif_list.o \
	jnif_errno.o \
	jnif_dlopen.o \
	jnif_sys.o \
	jnif_process.o



all: $(OFILES)
	c++ -dynamiclib -o libjnif.jnilib $(OFILES) -framework JavaVM

$(OFILES): jnif.h

jnif_resource.cc: erjang_EResource.h

erjang_EResource.h: $(ERJANG_JAR)
	javah -classpath $(ERJANG_JAR) erjang.EResource

jnif.cc: erjang_NIF.h

erjang_NIF.h: $(ERJANG_JAR)
	javah -classpath $(ERJANG_JAR) erjang.NIF

clean:
	-rm *.o erjang_*.h

