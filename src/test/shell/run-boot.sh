#!/bin/bash

OUTPUT_BASE="$1" # A base name

BASE_DIR=`dirname "$BASH_SOURCE"`"/../../.."

OUTPUT_FILE="$OUTPUT_BASE.csv"
OUTPUT_REL_FILE="$OUTPUT_BASE-relative.csv"

EJ_EXE="$BASE_DIR/ej"

CACHE_DIR="$BASE_DIR/boot-test-cache"

# Special cache location:
export ERJ_CACHE_DIR="$CACHE_DIR"

function error() {
    echo "*** $1" >&2
    exit 1
}

function measure() {
    local outfile="$1"; shift # Rest of arguments are passed to ej
    /usr/bin/time --format '%e\t%U\t%S\t%M' --output boot-stats.tmp1 \
      ./ej "$@" -noshell -sasl sasl_error_logger false -eval \
	'erlang:garbage_collect(),
         AA=hd([V || {T,_,V}<-erlang:system_info(allocated_areas), (T=='"'non_heap:Perm Gen'"' orelse T=='"'non_heap:PS Perm Gen'"')]),
         GHS=erlang:system_info(global_heaps_size),
         io:format("~b\t~b\n", [AA,GHS]),
         erlang:halt().' \
	< /dev/null > boot-stats.tmp2
    paste boot-stats.tmp1 boot-stats.tmp2 >> "$outfile"
}

# Prepare cache dir and ensure no old result files remain:
mkdir "$CACHE_DIR"
rm boot-measurement-{empty,populated,interpreted}.dat 2>/dev/null

# Run and measure:
for ((i=1; i<=3; i++)) ; do
    # Test from empty cache:
    measure boot-measurement-empty.dat

    # Test from populated cache:
    measure boot-measurement-populated.dat

    # Test interpreted mode:
    measure boot-measurement-interpreted.dat +i

    # Clean up:
    echo -n "Jar count: " ; ls -1 "$CACHE_DIR/.erjang/"*.jar | wc -l
    rm "$CACHE_DIR/.erjang/"*.{jar,ja#} 2>/dev/null
    rmdir "$CACHE_DIR/.erjang"
done

function compute() {
    local filenamepart="$1" legend="$2"
    perl -We '
      BEGIN {@sum=0; $cnt=0; $legend=$ARGV[0];}

      while (<STDIN>) {
        my $i=0;
        foreach (split("\t")) {$sum[$i++] += $_;}
        $cnt++;
      }

      sub max($$) {return ($_[0] > $_[1])? $_[0] : $_[1];}
      END {
        # Rearrange:
        # From: Elapsed, user, system, process size, permgen, global heap size
        # To: Elapsed, user+system, user, system, footprint, permgen.
        @sum = ($sum[0], $sum[1]+$sum[2], $sum[1], $sum[2], $sum[5]/1024, $sum[4]/1024);
        @avg = map {$_/$cnt} @sum;
        print "\"$legend - Elapsed time\",\"$legend - User+system time\",\"$legend - User time\",\"$legend - System time\",\"$legend - Process size\",\"$legend - PermGen size\"\n";
        print (join(",",@avg)."\n");
      }
    ' "$legend" < boot-measurement-$filenamepart.dat > "$OUTPUT_BASE-$filenamepart.csv"
}

compute empty "Empty module cache"
compute populated "Populated module cache"
compute interpreted "Interpreted mode"

# Clean up:
rmdir "$CACHE_DIR"

