#!/usr/bin/perl -W

# Static data:
my %TYPES_SUBTYPES =
    (
     'S' => ['c', 'x', 'y'],
     'D' => ['x', 'y'],
     'A' => ['c'],
     );
my %TYPES_OPERAND_CLASS =
    (
     'x' => "Operands.XReg",
     'y' => "Operands.YReg",
     'c' => "Operands.Literal",
     'L' => "Operands.Label"
     );
my %TYPES_DECODE =
    (
     'c' => "consts[#]",
     'x' => "stack[x0 + (#)]",
     'y' => "stack[fp - (#)]",
     'L' => "(#)"
     );
my %TYPES_ENCODE =
(
 'c' => "encode_literal(#)",
 'x' => "#.nr",
 'y' => "#.nr",
 'L' => "#.nr"
 );
# my %TYPES_JAVATYPE =
#     (
#      'c' => "EObject",
#      'x' => "EObject",
#      'y' => "EObject",
#      'L' => "int"
#      );
my %TYPES_ALLOWED_OPS =
    (
     'x' => {'GET'=>1, 'SET'=>1},
     'y' => {'GET'=>1, 'SET'=>1},
     'c' => {'GET'=>1},
     'L' => {'GOTO'=>1}
     );


my @METAS = ('GET', 'SET', 'GOTO');

my $enum_code  = "";
my $interp_code = "";
my $encoder_code = "";

sub reverse_map {
    my @a = @_;
    my %res = ();
    for (my $i=0; $i<=$#a; $i++) {$res{$a[$i]} = $i;}
    return %res;
}

sub subst {
    my ($template, $arg) = @_;
    $template =~ s/\#/$arg/g;
    return $template;
}

sub base_types {
    my ($type,$op) = @_;
    if (exists $TYPES_SUBTYPES{$type}) {
	my @res = ();
	for my $subtype (@{$TYPES_SUBTYPES{$type}}) {
	    push(@res, base_types($subtype, $op));
	}
	return @res;
    } else { # A scalar
	exists $TYPES_ALLOWED_OPS{$type}{$op} or
	    die "Operation '$op' not supported for type '$type'";
	return ($type);
    }
}

sub process_instruction {
    my ($insname, $directives,
	$argmap, $cls_arg_names, $cls_arg_types,
	$action, $code_acc, $varmap) = @_;
  again:
    my $eindent = "\t" x (1 + scalar keys %{$varmap});
    # First process all GETs, then all SETs, etc.:
    if ($action =~ /\b(GET)\((\w+)\)/ ||
	$action =~ /\b(SET)\((\w+),\s+/ ||
	$action =~ /\b(GOTO)\((\w+)\)/)
    {
	my ($macro,$arg) = ($1,$2);
	if (exists $varmap->{$arg}) { # Replacement is already known.
	    if ($macro eq 'GET') {
		my $replacement = $varmap->{$arg};
		$action = "$`$replacement$'";
	    } elsif ($macro eq 'SET') {
		my $replacement = $varmap->{$arg};
		$action = "$`$replacement = ($'";
	    } elsif ($macro eq 'GOTO') {
		my $replacement = $varmap->{$arg};
		$action = "$`ip = $replacement$'";
	    } else {die;}
	    goto again;
	} else { # Replacement is not known.
	    my $argno = $argmap->{$arg};
	    defined $argno or die "Undefined argument: $arg";
	    my $arg_src_name = $cls_arg_names->[$argno];
	    my $arg_type = $cls_arg_types->[$argno];

	    my @bts = base_types($arg_type, $macro);
	    my $first_bt = 1;
	    for my $base_type (@bts) {
		my $opClass = $TYPES_OPERAND_CLASS{$base_type};
		$encoder_code .= $eindent if ($first_bt);
		$encoder_code .= "if (typed_insn.$arg_src_name instanceof $opClass) {\n";
		$encoder_code .= $eindent."\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";
		my $encoding_exp_code = subst($TYPES_ENCODE{$base_type},
					      "typed_$arg");
		$encoder_code .= $eindent."\tcode[pos++] = $encoding_exp_code;\n";

		# Setup args for recursive call:
		my %new_varmap = %{$varmap};
		my $new_code_acc = $code_acc;

		my $decl = "int _$arg = insns[ip++];\n\t\t";
		$new_code_acc = "$code_acc$decl";
		$new_varmap{$arg} = subst($TYPES_DECODE{$base_type}, "_$arg");
		my $argno1 = 1+$argno;
		process_instruction("${insname}_$argno1$base_type", $directives,
				    $argmap, $cls_arg_names, $cls_arg_types,
				    $action, $new_code_acc, \%new_varmap);
		$encoder_code .= $eindent."} else ";
		$first_bt = 0;
	    }
	    $encoder_code .= "throw new Exception(\"Unrecognized operand\");\n";
	}
    } else {
	$enum_code .= "$insname,\n";
	$interp_code .= "\tcase $insname: {\n\t\t$code_acc$action\n\t} break;\n";
	$encoder_code .= "${eindent}code.set(opcode_pos, $insname);\n";
	print "DB| process_instruction $insname: Leaf: $code_acc$action.\n";
    }
}


sub parse() {
    my $cur_ins_class;
    my @cls_arg_names_ = ();
    my @cls_arg_types = ();
    $_ = <>; do {{
	chomp;
	s/\#.*//;
	if (/^$/) {
	    next;
	} elsif (/^%class (\w+)\(([^\)]*)\)$/) {
	    $cur_ins_class = $1;
	    my $tmp = $2;
	    @cls_arg_names = @cls_arg_types = ();
	    for my $arg (split(/\s+/, $tmp)) {
		die "Bad class field syntax" unless ($arg =~ /(\w+):(\w)/);
# 	    print "DB| arg: $arg => $1/$2\n";
		push(@cls_arg_names, $1);
		push(@cls_arg_types, $2);
	    }
	    print "DB| class: $cur_ins_class, @cls_arg_names, @cls_arg_types\n";
	} elsif (/^(\w+)\s+([\w\s]*):(.*)$/) {
	    my $insname = $1;
	    my $directives = $3;
	    my @args = split(/\s+/, $2);
	    my %argmap = reverse_map(@args);
	    die "Bad arg count" unless (scalar @args == scalar @cls_arg_names);
	    print "DB| instruction $insname...\n";
	    my $action = <>; chomp $action;
	    die unless ($action =~ /^\s/);
	    $action =~ s/^\s+//;
	    $encoder_code .= "case $insname: {\n";
	    process_instruction($insname, $directives,
				\%argmap, \@cls_arg_names, \@cls_arg_types,
				$action,
				"", {});
	    $encoder_code .= "\n} break;\n";
	} else {
	    die "Does not understand this line:\n\t$_\n";
	}
    }} while (<>);
}

sub emit {
    print "ENUM:\n$enum_code\n";
    print "INTERPRETER:\n$interp_code\n";
    print "ENCODER:\n$encoder_code\n";
}

parse();
emit();
