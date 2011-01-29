#!/bin/bash

BASE_DIR=`dirname "$BASH_SOURCE"`"/../../.."
ESTONE_DIR="$BASE_DIR/test_server"
RAW_OUTPUT_FILE="$BASE_DIR/estone.tmp"
OUTPUT_FILE="$BASE_DIR/test-outputs/estone.csv"

EJ_EXE="$BASE_DIR/ej"

function error() {
    echo "*** $1" >&2
    exit 1
}

# Compile the beam files:
echo "Compiling estone..." >&2
(export DIR="$ESTONE_DIR" ; "$EJ_EXE" -noshell -noinput -eval 'Dir=os:getenv("DIR"), Opts=[{i,Dir},{outdir,Dir},nowarn_unused_vars], [{ok,_} = c:c(filename:join(Dir,Mod++".erl"), Opts) || Mod <- ["ts","estone_SUITE"]], erlang:halt().') || error "Compiling EStone failed"

# Run the EStone suite 5 times:
echo "Running estone..." >&2
"$EJ_EXE" -pa "$ESTONE_DIR" -noinput -noshell -eval "[ts:run(estone) || _ <- [1,2,3,4,5]], erlang:halt()." > "$RAW_OUTPUT_FILE" || error "Running EStone failed"

cat "$RAW_OUTPUT_FILE" | perl -ne '
BEGIN{
  %numbers=();
}

if (/^\{[\"\x27]([^\"\x27]*)[\"\x27],(\d+)\}$/) {
  $numbers{$1} = [] unless exists($numbers{$1});
  push(@{$numbers{$1}}, $2);
#  print "$1: $2\n";
}

sub avg {
  my ($cnt,$sum)=(0,0); foreach (@_) {$sum+=$_; $cnt++;}
  return $sum/$cnt;
}

my @keys, @values;
sub add_kv {
  push(@keys, "\"$_[0]\"");
  push(@values, $_[1]);
}

END{
  { # EStones totals:
    my ($d1,$d2,@d) = @{$numbers{"ESTONES"}};
    my $hotavg = int(0.5 + avg(@d));
      # Three data points: first, second, and average-of-rest run.
      add_kv("EStones - first iteration",  $d1);
      add_kv("EStones - second iteration", $d2);
      add_kv("EStones - hot VM",           $hotavg);
  }

  for my $k (sort keys %numbers) {
    next if ($k eq "ESTONES");
    my ($d1,$d2,@d) = @{$numbers{$k}};
    my $hotavg = int(0.5 + avg(@d));
    add_kv($k, $hotavg);
  }

  print (join(",",@keys)."\n");
  print (join(",",@values)."\n");
}
' | tee "$OUTPUT_FILE"

