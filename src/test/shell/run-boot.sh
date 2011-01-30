#!/bin/bash

OUTPUT_BASE="$1" # A base name

BASE_DIR=`dirname "$BASH_SOURCE"`"/../../.."
RAW_OUTPUT_FILE="$BASE_DIR/estone.tmp"

OUTPUT_FILE="$OUTPUT_BASE.csv"
OUTPUT_REL_FILE="$OUTPUT_BASE-relative.csv"

EJ_EXE="$BASE_DIR/ej"

CACHE_DIR="$BASE_DIR/boot-test-cache"

# Special cache location:
export HOME="$CACHE_DIR"

function error() {
    echo "*** $1" >&2
    exit 1
}

function measure() {
    local outfile="$1"; shift # Rest of arguments are passed to ej
    /usr/bin/time --format '%e\t%U\t%S\t%M' --output "$outfile" --append \
    ./ej "$@" < /dev/null
}

# Prepare cache dir and ensure no old result files remain:
mkdir "$HOME"
rm boot-measurement-{empty,populated,interpreted}.dat 2>/dev/null

# Run and measure:
for ((i=1; i<=2 ; i++)) ; do
    # Test from empty cache:
    measure boot-measurement-empty.dat

    # Test from populated cache:
    measure boot-measurement-populated.dat

    # Test interpreted mode:
    measure boot-measurement-interpreted.dat +i

    # Clean up:
    rm "$CACHE_DIR/.erjang/"*.{jar,ja#} 2>/dev/null
done

for i in empty populated interpreted ; do
    perl -Wne '
      BEGIN {@sum=0; $cnt=0;}

      my $i=0;
      foreach (split("\t")) {$sum[$i++] += $_;}
      $cnt++;

      END {
        @avg = map {$_/$cnt} @sum;
        print "Elapsed,User,System,Memory\n";
        print (join(",",@avg)."\n");
      }
    ' boot-measurement-$i.dat > "$OUTPUT_BASE-$i.csv"
done


# Clean up:
rmdir "$CACHE_DIR"

