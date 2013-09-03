

JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
JAVA_ARCH=darwin
ERTS_HOME=/Users/krab/Projects/erjang/x86_64-apple-darwin12.4.0/erts-5.10.2


CXXFLAGS=-I${JAVA_HOME}/include \
	-I${JAVA_HOME}/include/${JAVA_ARCH} \
	-I${ERTS_HOME}/include \
	-fPIC

OFILES= jnif.o \
	jnif_env.o \
	jnif_binary.o \
	jnif_resource.o \
	jnif_lock.o \
	jnif_number.o \
	jnif_string.o

all: $(OFILES)
	c++ -dynamiclib -o libjnif.jnilib $(OFILES) -framework JavaVM

$(OFILES): jnif.h

clean:
	-rm *.o
