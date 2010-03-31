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
     'I' => "int",
     'L' => "Operands.Label",
     'E' => "ExtFun"
     );
my %TYPES_DECODE =
    (
     'c' => "consts[#]",
     'x' => "reg[#]",
     'y' => "stack[sp - (#)]",
     'I' => "(#)",
     'L' => "(#)",
     'E' => "ext_funs[#]"
     );
my %TYPES_ENCODE =
(
 'c' => "encodeLiteral(#)",
 'x' => "#.nr",
 'y' => "#.nr",
 'I' => "#",
 'L' => "#.nr",
 'E' => "encodeExtFun(#)"
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
     'I' => {'GET'=>1},
     'L' => {'GOTO'=>1},
     'E' => {'GET'=>1}
     );

my %PRIMITIVE_TYPES = ('I' => 1);

my @METAS = ('GET', 'SET', 'GOTO');

my $enum_code  = "";
my $interp_code = "";
my $encoder_code = "";
my $enum_count = 0;

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

sub multi_subst {
    my ($template, $map) = @_;
    $template =~ s/\#(\w+)\#/$map->{$1} or die "Don't know what to substitute for #$1#"/ge;
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

sub process_instruction_rec {
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
		$action = "$`pc = $replacement$'";
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
		if (exists $PRIMITIVE_TYPES{$base_type}) {
		    $encoder_code .= "/*Prim: $base_type*/";
		    $encoder_code .= "\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";
		} else {
		    $encoder_code .= "/*Nonprim: $base_type*/";
		    $encoder_code .= "if (typed_insn.$arg_src_name instanceof $opClass) {\n";
		    $encoder_code .= $eindent."\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";
		}
		my $encoding_exp_code = subst($TYPES_ENCODE{$base_type},
					      "typed_$arg");
		$encoder_code .= $eindent."\temit($encoding_exp_code);\n";

		# Setup args for recursive call:
		my %new_varmap = %{$varmap};
		my $new_code_acc = $code_acc;

		my $decl = "int _$arg = code[pc++];\n\t\t";
		$new_code_acc = "$code_acc$decl";
		$new_varmap{$arg} = subst($TYPES_DECODE{$base_type}, "_$arg");
		my $argno1 = 1+$argno;
		process_instruction_rec("${insname}_$argno1$base_type", $directives,
				    $argmap, $cls_arg_names, $cls_arg_types,
				    $action, $new_code_acc, \%new_varmap);
		if (exists $PRIMITIVE_TYPES{$base_type}) {
		    $encoder_code .= $eindent."// ";
		} else {
		    $encoder_code .= $eindent."} else ";
		}
		$first_bt = 0;
	    }
	    $encoder_code .= "throw new Error(\"Unrecognized operand\");\n";
	}
    } else {
#	$enum_code .= "$insname,\n";
	if ($action =~ /^\s*\{\s*\}\s*$/) {
	    print STDERR "DB| nop: $insname\n";
	    $encoder_code .= "${eindent}nop(opcode_pos);";
	} else {
	    $enum_code .= "\tpublic static final short $insname = $enum_count;\n"; $enum_count++;
	    $interp_code .= "\tcase $insname: {\n".
		"\t\t$code_acc$action\n".
		"\t} break;\n";
	    $encoder_code .= "${eindent}emitAt(opcode_pos, $insname);\n";
	    print "DB| process_instruction_rec $insname: Leaf: $code_acc$action.\n";
	}
    }
}

sub process_instruction {
    my ($insname, $directives,
	$argmap, $cls_arg_names, $cls_arg_types,
	$action) = @_;

    my $code_acc = "";
    my $varmap = {};

    # Process directives:
    while ($directives ne '') {
	print STDERR "DB| dir=$directives\n";
	if ($directives =~ /^\s+/) {}
	elsif ($directives =~ /^encode\(([^()]*)\)\((\w+)\)/) {
	    my ($encode_exp,$arg) = ($1,$2);
	    $encoder_code .= "emit($encode_exp);\n";
	    my $decl = "int _$arg = code[pc++];\n\t\t";
	    $code_acc .= $decl;
	    $varmap->{$arg} = "_$arg";
	    print STDERR "DB| custom encode: $encode_exp // $arg\n";

	} elsif ($directives =~ /^encoder_side_effect\(([^()]*)\)/) {
	    my ($encoder_stm) = ($1);
	    $encoder_code .= "{$encoder_stm}\n";
	} else {die "Invalid directive: $directives";}
	$directives = $'; next; #'
    }
    return process_instruction_rec($insname, $directives,
				   $argmap, $cls_arg_names, $cls_arg_types,
				   $action,
				   $code_acc, $varmap);
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
	    $cur_ins_class = "Insn.$cur_ins_class" unless ($cur_ins_class eq 'Insn');
	    my $tmp = $2;
	    @cls_arg_names = @cls_arg_types = ();
	    for my $arg (split(/\s+/, $tmp)) {
		die "Bad class field syntax ($tmp)" unless ($arg =~ /(\w+):(\w)/);
# 	    print "DB| arg: $arg => $1/$2\n";
		push(@cls_arg_names, $1);
		push(@cls_arg_types, $2);
	    }
	    print "DB| class: $cur_ins_class, @cls_arg_names, @cls_arg_types\n";
	} elsif (/^(\w+)\s*([\w\s]*):(.*)$/) {
	    my $insname = $1;
	    my $directives = $3;
	    my @args = split(/\s+/, $2);
	    my %argmap = reverse_map(@args);
	    die "Bad arg count" unless (scalar @args == scalar @cls_arg_names);
	    print "DB| instruction $insname...\n";
	    my $action = <>; chomp $action;
	    die unless ($action =~ /^\s/);
	    $action =~ s/^\s+//;
	    $encoder_code .= "case $insname: {\n".
		"\t$cur_ins_class typed_insn = ($cur_ins_class) insn;\n";
	    process_instruction($insname, $directives,
				\%argmap, \@cls_arg_names, \@cls_arg_types,
				$action);
	    $encoder_code .= "\n} break;\n";
	    $enum_count = int($enum_count/100+1)*100;# DEBUG
	} else {
	    die "Does not understand this line:\n\t$_\n";
	}
    }} while (<>);
}

sub readFile {
    my ($filename) = @_;
    open FIL,$filename or die "Error opening file: $!";
    local $/;
    my $data = <FIL>;
    close FIL;
    return $data;
}

sub writeFile($$) {
    my ($filename,$data) = @_;
    open FIL,">",$filename or die "Error opening file: $!";
    print FIL $data;
    close FIL;
}

sub emit {
    my $encoder_template = readFile("Interpreter.template");
    $enum_code .= "\tpublic static final short MAX_OPCODE = $enum_count;\n";
    print "ENUM:\n$enum_code\n";
    print "INTERPRETER:\n$interp_code\n";
    print("ENCODER:\n".subst($encoder_template,$encoder_code)."\n");
    my $subst_map = {'ENCODE' => $encoder_code,
		     'ENUM' => $enum_code,
		     'INTERPRET' => $interp_code};
    writeFile("Interpreter.java", multi_subst($encoder_template,$subst_map));
}

parse();
emit();
