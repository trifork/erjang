#!/usr/bin/perl -W

# Static data:
my %TYPES_SUBTYPES =
    (
     'S' => ['c', 'x', 'y'],
     'D' => ['x', 'y'],
     'A' => ['c'],
     'L0' => ['L', 'nolabel'], # Label, possibly nofail
     'EG' => ['G', 'E']        # Ext. fun, possibly a guard
     );
my %TYPES_OPERAND_CLASS =
    (
     'x' => "Operands.XReg",
     'y' => "Operands.YReg",
     'c' => "Operands.Literal",
     'I' => "int",
     'IL' => "int", # Integer-as-label
     'L' => "Operands.Label",
     'nolabel' => ["Operands.Label", "# == null"],
     'E' => "ExtFun",
     'G' => ["ExtFun", "# instanceof ExtFun && \$onFail != null"],
     'JV' => "Operands.SelectList",
     'JA' => "Operands.SelectList"
     );
my %TYPES_DECODE =
    (
     'c' => "consts[#]",
     'x' => "reg[#]",
     'y' => "stack[sp - (#)]",
     'I' => "(#)",
     'IL' => "(#)",
     'L' => "(#)",
     'nolabel' => "nofailLabel()",
     'E' => "ext_funs[#]",
     'G' => "ext_funs[#]",
     'JV' => "value_jump_tables[#]",
     'JA' => "arity_jump_tables[#]"
     );
my %TYPES_ENCODE =
(
 'c' => "encodeLiteral(#)",
 'x' => "#.nr",
 'y' => "#.nr",
 'I' => "#",
 'IL' => "encodeLabel(#)",
 'L' => "encodeLabel(#.nr)",
 'E' => "encodeExtFun(#)",
 'G' => "encodeGuardExtFun(#)",
 'JV' => "encodeValueJumpTable(#)",
 'JA' => "encodeArityJumpTable(#)"
 );
my %DECODE_STEP_TEMPVAR = (
    'S' => 'term'
    );
my %TYPES_ALLOWED_OPS =
    (
     'x' => {'GET'=>1, 'SET'=>1},
     'y' => {'GET'=>1, 'SET'=>1},
     'c' => {'GET'=>1},
     'I' => {'GET'=>1},
     'IL' => {'GOTO'=>1, 'GET_PC'=>1},
     'L' => {'GOTO'=>1, 'GET_PC'=>1},
     'nolabel' => {'GOTO'=>1},
     'E' => {'GET'=>1, 'IS_GUARD'=>0},
     'G' => {'GET'=>1, 'IS_GUARD'=>1},
     'JV' => {'TABLEJUMP'=>1},
     'JA' => {'TABLEJUMP'=>1}
     );

my %PRIMITIVE_TYPES = ('I' => 1, 'IL'=>1);

my @METAS = ('GET', 'SET', 'GOTO', 'TABLEJUMP', 'GET_PC', 'IS_GUARD');

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
    my $back_subst_map = {};
    # Register and remove back-substitution declarations:
    $template =~ s/\#(\w+)<-(\w+)\(([^()]*)\)\s*(.*)\#/$back_subst_map->{$1}->{$2} = [$3,$4]; ""/ge;
    $template =~ s/\#(\w+)\#/(exists $map->{$1})? back_subst($map->{$1}, $back_subst_map->{$1}) : (die "Don't know what to substitute for #$1#")/ge;
    return $template;
}

my $BALANCED_RE = '(?:(?(DEFINE)(?<BALANCED>[^()]+|\((?&BALANCED)\)))([^()]+|\((?&BALANCED)\))+)';
my $BALANCED_NO_TOPLEVEL_COMMA_RE = '(?:(?(DEFINE)(?<BALANCED>[^()]+|\((?&BALANCED)\)))([^(),]+|\((?&BALANCED)\))+)';
sub back_subst {
    my ($subject, $subst_map) = @_;
    foreach $macro (keys %{$subst_map}) {
	my @macro_info = @{$subst_map->{$macro}};
	if (scalar @macro_info == 1) { # Argument-less macro
	    my ($body) = @macro_info;
	    $subject =~ s/\b$macro\b/$body/ge;
	} else {
	    my ($formals, $body) = @macro_info;
	    my @formals = split(/\s*,\s*/, $formals);
	    # Substitute macro "$macro":
	    $subject =~ s/\b$macro\b\(($BALANCED_RE?)\)/{
my @actuals = split_into_args($1);
die "Wrong number of arguments to macro '$macro' (expected ".scalar @formals.", found ".scalar @actuals.")" unless ($#actuals==$#formals);
my $arg_map = make_map(\@formals, \@actuals);
back_subst($body, $arg_map)
            }/ge; # /
	}
    }
    return $subject;
}

sub split_into_args {
    my ($s) = @_;
    my @res = ();
    while ($s =~ /($BALANCED_NO_TOPLEVEL_COMMA_RE),?/g) {push(@res,$1);}
    return @res;
}

sub make_map {
    my ($key_list, $value_list) = @_;
    my %res = ();
    my $last_index = scalar(@{$key_list}-1);
    for my $i (0..$last_index) {$res{$key_list->[$i]} = [$value_list->[$i]];}
    return \%res;
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

=item process_instruction_rec()
Recursive processing of instruction body.
This generates both the encoding code and the interpreter action code(s)
for the instruction in question.
Each instruction spec may give rise to more than one variant, with
different encodings and action code, as determined by (primarily)
%TYPES_SUBTYPES for the involved parameters.
=cut
sub process_instruction_rec {
    my ($insname, $directives,
	$argmap, $cls_arg_names, $cls_arg_types,
	$action, $code_acc, $varmap) = @_;
  again:
    my $eindent = "\t" x (1 + scalar keys %{$varmap});
    # First process all GETs, GET_PCs and IS_GUARDs, then the rest:
    if ($action =~ /\b(IS_GUARD)\((\w+)\)/ ||
	$action =~ /\b(GET)\((\w+)\)/ ||
	$action =~ /\b(GET_PC)\((\w+)\)/ ||
	$action =~ /\b(SET)\((\w+),\s+/ ||
	$action =~ /\b(GOTO)\((\w+)\)/ ||
	$action =~ /\b(TABLEJUMP)\((\w+),\s+/)
    {
	my ($macro,$arg) = ($1,$2);
	if (exists $varmap->{$arg}) { # Replacement is already known.
	    $action = "$`".access_expr($macro, $varmap->{$arg})."$'";
	    goto again;
	} else { # Replacement is not known.
	    my $argno = $argmap->{$arg};
	    defined $argno or die "Undefined argument: $arg";
	    my $arg_src_name = $cls_arg_names->[$argno];
	    my $arg_type = $cls_arg_types->[$argno];

	    my @bts = base_types($arg_type, $macro);
	    my $first_bt = 1;
	    for my $base_type (@bts) {
		my $encoding_needed = ($TYPES_DECODE{$base_type} =~ /\#/);

		my $emitted = 0;
		my $new_code_acc = $code_acc;

		$encoder_code .= $eindent if ($first_bt);
		my $opClass = $TYPES_OPERAND_CLASS{$base_type};
		my $test_exp;
		if (ref($opClass) eq 'ARRAY') { # Special test
		    my @test_spec = @{$opClass};
		    $opClass = $test_spec[0];
		    $test_exp = $test_spec[1];
		    $test_exp =~ s/\#/typed_insn.$arg_src_name/g;
		    $test_exp =~ s/\$([\w\d]+)/typed_insn.$cls_arg_names->[$argmap->{$1}]/g;
		    $encoder_code .= "/*Special: $base_type*/";
		} else {
		    $test_exp = "typed_insn.$arg_src_name instanceof $opClass";
		}

		if (exists $PRIMITIVE_TYPES{$base_type}) {
		    $encoder_code .= "/*Prim: $base_type*/";
		    $encoder_code .= "\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";
		} else {
		    $encoder_code .= "/*Nonprim: $base_type*/";
		    $encoder_code .= "if ($test_exp) {\n";
		    $encoder_code .= $eindent."\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";
		}

		if ($encoding_needed) {
		    my $encoding_exp_code = subst($TYPES_ENCODE{$base_type},
						  "typed_$arg");
		    $encoder_code .= $eindent."\temit($encoding_exp_code);\n";
		    $emitted++;

		    my $decl = "int _$arg = code[pc++];\n\t\t";
# 		    $decl .= "System.err.println(\"DB| fetch: _$arg = \"+_$arg);\n\t\t";

		    $new_code_acc .= $decl;
		}

		# Setup args for recursive call:
		my %new_varmap = %{$varmap};

		$new_varmap{$arg} = subst($TYPES_DECODE{$base_type}, "_$arg");
		my $argno1 = $argno + $emitted;
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
	    $encoder_code .= "throw new Error(\"Unrecognized operand: \"+typed_insn.$arg_src_name);\n";
	}
    } else { # No more macros to process.
#	$enum_code .= "$insname,\n";
	if ($action =~ /^\s*\{\s*\}\s*$/) {
	    $encoder_code .= "${eindent}nop(opcode_pos);";
	} else {
	    $enum_code .= "\tpublic static final short $insname = $enum_count;\n"; $enum_count++;
	    # The "if (true)" wrapping in the following line is present in
	    # order to eliminate 'unreachable' compiler warnings.
	    $interp_code .= "\tcase $insname: if (true) {\n".
		"\t\t$code_acc$action\n".
		"\t} break;\n";
	    $encoder_code .= "${eindent}emitAt(opcode_pos, $insname);\n";
	}
    }
}

sub access_expr($$) {
    my ($macro, $var) = @_;
    if ($macro eq 'GET' || $macro eq 'GET_PC') {
	return $var;
    } elsif ($macro eq 'IS_GUARD') {
	return $var ? "true" : "false";
    } elsif ($macro eq 'SET') {
	return "$var = (";
    } elsif ($macro eq 'GOTO') {
	return "pc = $var";
    } elsif ($macro eq 'TABLEJUMP') {
	return "pc = $var.lookup(";
    } else {die;}
}

=item process_instruction()
Processes a BEAM instruction, generating Java code for both the
encoding and interpreting part for the instruction in question.
Each instruction spec may give rise to more than one variant.
Generating all variants is done by a recursive process involving
process_instruction_rec().
=cut
sub process_instruction {
    my ($insname, $directives,
	$argmap, $cls_arg_names, $cls_arg_types,
	$action) = @_;

    my $code_acc = "";
    my $varmap = {};

    # Process directives:
    while ($directives ne '') {
	if ($directives =~ /^\s+/) {}
	elsif ($directives =~ /^encode\(([^()]*)\)\((\w+)\)/) {
	    my ($encode_exp,$arg) = ($1,$2);
	    $encoder_code .= "emit($encode_exp);\n";
	    my $decl = "int _$arg = code[pc++];\n\t\t";
	    $code_acc .= $decl;
	    $varmap->{$arg} = "_$arg";

	} elsif ($directives =~ /^encoder_side_effect\(([^()]*)\)/) {
	    my ($encoder_stm) = ($1);
	    $encoder_code .= "{$encoder_stm}\n";
	} else {die "Invalid directive: $directives";}
	$directives = $'; next; #'
    }

    my @macro_calls = find_and_order_macro_calls($action, $varmap, $argmap,
						 $cls_arg_types);

    return process_instruction_rec($insname, $directives,
				   $argmap, $cls_arg_names, $cls_arg_types,
				   $action,
				   $code_acc, $varmap);
}

sub find_and_order_macro_calls($$$$) {
    my ($action, $varmap, $argmap, $cls_arg_types) = @_;
    my @macro_calls = ();
    while ($action =~ /\b(IS_GUARD|GET|GET_PC|GOTO|SET|TABLEJUMP)\((\w+)[\),]/g) {
	push(@macro_calls, [$1,$2]);
    }

    @macro_calls = sort {macro_order($a, $varmap, $argmap, $cls_arg_types) <=>
			 macro_order($b, $varmap, $argmap, $cls_arg_types)
    } @macro_calls;

    my @pretty = map {join(",",@{$_})} @macro_calls;
#    print STDERR ("DB| Macro order for $insname: @pretty\n");
    return @macro_calls;
}


=item macro_order()
Used for prioritizing macro calls.
=cut
sub macro_order($$$) {
    my ($macro_call, $varmap, $argmap, $cls_arg_types) = @_;
    my ($macro_name,$macro_arg) = @{$macro_call};

    # Actions go last:
    return 9 if ($macro_name eq 'SET' ||
		 $macro_name eq 'GOTO' ||
		 $macro_name eq 'TABLEJUMP');

    # Preexisting variables shouldn't matter:
    return -1 if (exists $varmap->{$macro_arg});

    # Parameters which may be decoded by a separate decoding step go first:
    {
	my $argno = $argmap->{$macro_arg};
	defined $argno or die "Undefined argument: $macro_arg";
	my $arg_type = $cls_arg_types->[$argno];
	return 1 if (exists $DECODE_STEP_TEMPVAR{$arg_type});
    }

    # The rest go in the middle:
    return 5;
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
		die "Bad class field syntax ($tmp)" unless ($arg =~ /^([\w.\[\]]+):(\w[\w\d]?),?$/);
		push(@cls_arg_names, $1);
		push(@cls_arg_types, $2);
	    }
	} elsif (/^(\w+)\s*([\w\s]*):(.*)$/) {
	    my $insname = $1;
	    my $directives = $3;
	    my @args = split(/\s+/, $2);
	    my %argmap = reverse_map(@args);
	    die "Bad arg count" unless (scalar @args == scalar @cls_arg_names);

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
    my $subst_map = {'ENCODE' => $encoder_code,
		     'ENUM' => $enum_code,
		     'INTERPRET' => $interp_code};
    writeFile("Interpreter.java", multi_subst($encoder_template,$subst_map));
}

parse();
emit();
