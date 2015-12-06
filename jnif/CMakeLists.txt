cmake_minimum_required (VERSION 2.6)
project(jnif)

# Find stuff:
find_package(JNI)
if (JNI_FOUND)
    message (STATUS "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")
    message (STATUS "JNI_LIBRARIES=${JNI_LIBRARIES}")
    message (STATUS "javah=${Java_JAVAH_EXECUTABLE}")
endif()

# This detection is quite crude, but follows "build.xml":
if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  set(OTP_ROOT "../x86_64-unknown-linux-gnu/")
else()
if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(OTP_ROOT "../x86_64-apple-darwin12.4.0/")
endif()
endif()
set(ERTS_HOME "${OTP_ROOT}/erts-5.10.2")
message (STATUS "OTP_ROOT=${OTP_ROOT}")
message (STATUS "ERTS_HOME=${ERTS_HOME}")

# Define targets:
add_library(jnif SHARED
    jnif.h

    jnif.cc
    jnif_atom.cc
    jnif_binary.cc
    jnif_dlopen.cc
    jnif_env.cc
    jnif_errno.cc
    jnif_list.cc
    jnif_lock.cc
    jnif_number.cc
    jnif_process.cc
    jnif_resource.cc
    jnif_string.cc
    jnif_sys.cc
    jnif_thread.cc
    jnif_tuple.cc)

include_directories(${JNI_INCLUDE_DIRS} ${ERTS_HOME}/include)

# Handle specific environments:
if(CMAKE_COMPILER_IS_GNUCXX)
  list(APPEND CMAKE_CXX_FLAGS "-std=c++0x")
endif()
