#!/bin/bash

OUTPUT_BASE="$1" # A base name

BASE_DIR=`dirname "$BASH_SOURCE"`"/../../.."
ESTONE_DIR="$BASE_DIR/test_server"
RAW_OUTPUT_FILE="$BASE_DIR/estone.tmp"

OUTPUT_FILE="$OUTPUT_BASE.csv"
OUTPUT_REL_FILE="$OUTPUT_BASE-relative.csv"

EJ_EXE="$BASE_DIR/ej"

function error() {
    echo "*** $1" >&2
    exit 1
}

# Compile the beam files:
echo "Compiling estone..." >&2
(export DIR="$ESTONE_DIR" ; "$EJ_EXE" -noshell -noinput -eval 'Dir=os:getenv("DIR"), Opts=[{i,Dir},{outdir,Dir},nowarn_unused_vars], [{ok,_} = c:c(filename:join(Dir,Mod++".erl"), Opts) || Mod <- ["ts","estone_SUITE"]], erlang:halt().') || error "Compiling EStone failed"

# Run the EStone suite 8 times:
echo "Running estone..." >&2
"$EJ_EXE" -pa "$ESTONE_DIR" -noinput -noshell -eval "[ts:run(estone) || _ <- [1,2,3,4,5,6,7,8]], erlang:halt()." > "$RAW_OUTPUT_FILE" || error "Running EStone failed"

cat "$RAW_OUTPUT_FILE" | perl -Wne '
BEGIN{
  %reference_numbers = (
        "EStones - hot VM" => 190864,
	"Alloc and dealloc" => 5327,
	"Bif dispatch" => 52608,
	"Binary handling" => 6453,
	"Float arithmetics" => 2468,
	"Function calls" => 16422,
	"Generic server (with timeout)" => 8556,
	"Links" => 865,
	"Small Integer arithmetics" => 4722,
	"Timers" => 4319,
	"Work with large dataset" => 5502,
	"Work with large local dataset" => 5584,
	"ets datadictionary" => 10574,
	"huge messages" => 4886,
	"list manipulation" => 13483,
	"medium messages" => 13956,
	"pattern matching" => 17753,
	"small messages" => 11417,
	"traverse" => 5969
   );

  %numbers=();

  open(RELOUT,">&3");
}

if (/^\{[\"\x27]([^\"\x27]*)[\"\x27],([\d.]+)\}$/) {
  $numbers{$1} = [] unless exists($numbers{$1});
  push(@{$numbers{$1}}, $2);
#  print "$1: $2\n";
}

sub avg {
  my ($cnt,$sum)=(0,0); foreach (@_) {$sum+=$_; $cnt++;}
  return $sum/$cnt;
}

my (@keys, @values, @rel_keys, @rel_values);
sub add_kv {
  my ($k,$v) = @_;
  push(@keys, "\"$k\"");
  push(@values, sprintf("%.2f", $v));
  if (exists $reference_numbers{$k}) {
    push(@rel_keys, "\"$k\"");
    push(@rel_values, sprintf("%6.3f", 100*($v/$reference_numbers{$k})));
  }
}

sub remove_extremes {
  my ($d) = (@_);
  if (scalar @{$d} > 3) {
    # Throw away the extremes:
    @{$d}=sort @{$d};
    shift @{$d}; pop @{$d};
  }
}

END{
  { # EStones totals:
    my ($d1,$d2,@d) = @{$numbers{"ESTONES"}};
    remove_extremes(\@d);
    my $hotavg = avg(@d);
      # Three data points: first, second, and average-of-rest run.
      add_kv("EStones - first iteration",  $d1);
      add_kv("EStones - second iteration", $d2);
      add_kv("EStones - hot VM",           $hotavg);
  }

  for my $k (sort keys %numbers) {
    next if ($k eq "ESTONES");
    my ($d1,$d2,@d) = @{$numbers{$k}};
    remove_extremes(\@d);
    my $hotavg = avg(@d);
    add_kv($k, $hotavg);
  }

  print (join(",",@keys)."\n");
  print (join(",",@values)."\n");

  print RELOUT (join(",",@rel_keys)."\n");
  print RELOUT (join(",",@rel_values)."\n");
}
' > "$OUTPUT_FILE" 3>"$OUTPUT_REL_FILE"

