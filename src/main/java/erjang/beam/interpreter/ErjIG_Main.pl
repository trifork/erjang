#!/usr/bin/perl -W
use strict;
use ErjIG_Util;
use ErjIG_Operands;
use ErjIG_Prefetching;

my $enum_code  = "";
my $interp_code = "";
my $encoder_code = "";
my $enum_count = 0;
my $current_insname = "";

my $ORDER_PRE_ENCODE = 1; # Constant (TODO)


=item Per-instruction processing algorithm
Per-instruction processing algorithm:
 1. Process directives. Identify any pre-defined variables present in them.
 2. Identify all macro calls with the action string.
 3. Identify all variables read by the macros.
    These fall into the categories:
    - Predefined variables (?)
    - Pre-encoded variables - are encoded in a separate fetch instruction
    - Other variables - are encoded as part of the actual instruction.
 4. If any pre-encoded variables exist:
    Generate code for encoding the fetch instruction.
 5. Generate code for encoding and decoding the action instruction.
    Multiple variants may exist, so this is done recursively.
    Variable names have already been chosen, though.
=cut

=item process_instruction()
Processes a BEAM instruction, generating Java code for both the
encoding and interpreting part for the instruction in question.
Each instruction spec may give rise to more than one variant.
Generating all variants is done by a recursive process involving
process_instruction_rec().

Parameters:
* $insname: The name of the instruction.
* $directives: a list of special directives.
* $action: The action string of the instruction
* $varmap: The map of pre-defined variables;
* $ins_arg_map: Map from instruction parameter names to positions.
* $ins_arg_types: The list of instruction parameter types.
=cut
sub process_instruction {
    my ($insname, $directives,
	$ins_arg_map, $ins_arg_names, $ins_arg_types,
	$action) = @_;

    $current_insname = $insname;
    my $code_acc = "";
    my $varmap = {};
    my $encoder_code_after_prefetch = "";

    # Process directives:
    while ($directives ne '') {
	if ($directives =~ /^\s+/) {}
	elsif ($directives =~ /^encode\(([^()]*)\)\((\w+)\)/) {
	    my ($encode_exp,$arg) = ($1,$2);
	    $encoder_code_after_prefetch .= "emit($encode_exp);\n";
	    my $decl = "int _$arg = code[pc++];\n\t\t";
	    $code_acc .= $decl;
	    $varmap->{$arg} = "_$arg";

	} elsif ($directives =~ /^encoder_side_effect\(([^()]*)\)/) {
	    my ($encoder_stm) = ($1);
	    $encoder_code_after_prefetch .= "{$encoder_stm}\n";
	} else {die "Invalid directive: $directives";}
	$directives = $'; #'
    }

    my ($fetch_args, $macro_calls) =
	find_and_order_macro_calls($action, $varmap, $ins_arg_map, $ins_arg_types);

    $encoder_code .= process_instruction_prefetching($fetch_args, $ins_arg_map, $ins_arg_names);
    $encoder_code .= $encoder_code_after_prefetch;
    {my $i=0; foreach (@{$fetch_args}) {
	my $fetch_arg_name = $_->[0];
	$encoder_code .= "// DB| fetch_arg: $fetch_arg_name\n";
	$varmap->{$fetch_arg_name} = "prefetched".++$i;
     }}

    process_instruction_rec($insname, $directives,
			    $ins_arg_map, $ins_arg_names, $ins_arg_types,
			    $action,
			    $code_acc, $varmap);
}


sub process_instruction_prefetching($$) {
    my ($fetch_args, $ins_arg_map, $ins_arg_names) = @_;
    my @fetch_args = @{$fetch_args};
    return "" unless (@fetch_args);

    my @arg_classes = map {my ($name,$type) = @{$_}; $type} @fetch_args;
    my @arg_var_names = map {my ($name,$type) = @{$_}; "typed_insn.".$ins_arg_names->[$ins_arg_map->{$name}]} @fetch_args;
    my $prefetch_opcode_func =
	ErjIG_Prefetching::ensureGenerated_encodePrefetch(@arg_classes);
    return "int prefetch_opcode = $prefetch_opcode_func(".join(", ",@arg_var_names).");\n".
	"emitAt(opcode_pos, prefetch_opcode);\n".
	"opcode_pos = emitPlaceholder();\n";
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
	$argmap, $ins_arg_names, $ins_arg_types,
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
	    $encoder_code .= "//DB| \$varmap{$arg}\n";
	    my $argno = $argmap->{$arg};
	    defined $argno or die "Undefined argument: $arg";
	    my $arg_src_name = $ins_arg_names->[$argno];
	    my $arg_type = $ins_arg_types->[$argno];

	    my @bts = ErjIG_Operands::base_types($arg_type, $macro);
	    my $first_bt = 1;
	    for my $base_type (@bts) {
		my $emitted = 0;
		my $new_code_acc = $code_acc;

		$encoder_code .= $eindent if ($first_bt);
		my $opClass = $ErjIG_Operands::TYPES_OPERAND_CLASS{$base_type};
		my $test_exp;
		($test_exp, $opClass) = ErjIG_Operands::basetype_test_exp($base_type, "typed_insn.$arg_src_name");
		$test_exp =~ s/\$([\w\d]+)/typed_insn.$ins_arg_names->[$argmap->{$1}]/g;

		$encoder_code .= "if ($test_exp) {\n";
		$encoder_code .= $eindent."\t$opClass typed_$arg = ($opClass)typed_insn.$arg_src_name;\n";

		my $encoding_exp_code = ErjIG_Operands::basetype_encoding_exp($base_type, "typed_$arg");
		if ($encoding_exp_code ne "") {
		    $encoder_code .= $eindent."\temit($encoding_exp_code);\n";
		    $emitted++;

		    my $decl = "int _$arg = code[pc++];\n\t\t";
# 		    $decl .= "System.err.println(\"DB| fetch: _$arg = \"+_$arg);\n\t\t";

		    $new_code_acc .= $decl;
		}

		# Setup args for recursive call:
		my %new_varmap = %{$varmap};

		$new_varmap{$arg} = ErjIG_Operands::basetype_decoding_exp($base_type, "_$arg");
		my $argno1 = $argno + $emitted;
		process_instruction_rec("${insname}_$argno1$base_type", $directives,
				    $argmap, $ins_arg_names, $ins_arg_types,
				    $action, $new_code_acc, \%new_varmap);
		$encoder_code .= $eindent."} else ";
		$first_bt = 0;
	    }
	    $encoder_code .= "throw new Error(\"Unrecognized operand: \"+typed_insn.$arg_src_name);\n";
	}
    } else { # No more macros to process.
	if ($action =~ /^\s*\{\s*\}\s*$/) {
	    $encoder_code .= "${eindent}nop(opcode_pos);";
	} else {
	    $enum_code .= "\tpublic static final short OPC_$insname = $enum_count;\n"; $enum_count++;
	    # The "if (true)" wrapping in the following line is present in
	    # order to eliminate 'unreachable' compiler warnings.
	    $interp_code .= "\tcase OPC_$insname: if (true) {\n".
		"\t\t$code_acc$action\n".
		"\t} break;\n";
	    $encoder_code .= "${eindent}emitAt(opcode_pos, OPC_$insname);\n";
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


=item find_and_order_macro_calls()
Parameters:
* $action: The action string of the instruction
* $varmap: The map of pre-defined variables;
* $ins_arg_map: The map of instruction parameter names to positions.
* $ins_arg_types: The list of instruction parameter types.
Returns:
* \@fetch_args: Descriptions (name,type) of the variables to be fetched.
* \@macro_calls: Descriptions (macro_name,parameter_name,parameter_type) of
  the macros used.
=cut
sub find_and_order_macro_calls($$$$) {
    my ($action, $varmap, $ins_arg_map, $ins_arg_types) = @_;
    my @predef = ();
    my @macro_calls = ();
    while ($action =~ /\b(IS_GUARD|GET|GET_PC|GOTO|SET|TABLEJUMP)\((\w+)[\),]/g) { # Action parameter access is a fetch
	my ($macro_name, $param_name) = ($1,$2);

	# Preexisting variable?
	if (exists $varmap->{$param_name}) {
	    push(@predef, [$macro_name, $param_name, 'I']);
	} else {
	    my $argno = $ins_arg_map->{$param_name};
	    defined $argno or die "Undefined argument: $param_name";

	    my $param_type = $ins_arg_types->[$argno];

	    push(@macro_calls, [$macro_name, $param_name, $param_type]);
	}
    }

    my %marked = %{$varmap};
    my @fetch_args  = ();
    my @action_args = ();

    # Parameters which may be decoded by a separate decoding step go first:
    foreach my $mc (@macro_calls) {
	my @tmp = @{$mc};
	my ($macro_name, $param_name, $param_type) = @{$mc};
	next if (exists $marked{$param_name});
	if (exists $ErjIG_Operands::DECODE_STEP_TEMPVAR{$param_type}) {
	    push(@fetch_args, [$param_name,$param_type]);
	    $marked{$param_name} = 1;
	}
    }

    # The rest of the parameters go last:
    foreach my $mc (@macro_calls) {
	my ($macro_name, $param_name, $param_type) = @{$mc};
	next if (exists $marked{$param_name});
	push(@action_args, [$param_name, $param_type]);
	$marked{$param_name} = 1;
    }

    my @pretty = map {join(",",@{$_})} @macro_calls;
    my @pretty2 = map {join(",",@{$_})} @fetch_args;

    @macro_calls = (@predef, @macro_calls);
    return (\@fetch_args, \@predef,\@macro_calls, );
}


=item macro_order()
Used for prioritizing macro calls.
=cut
sub macro_order($) {
    my ($macro_call) = @_;
    my ($macro_name, $param_name, $param_type) = @{$macro_call};
    print STDERR "DB| macro_order: ".join(":",@{$macro_call})."\n";

    # Actions go last:
    return 9 if ($macro_name eq 'SET' ||
		 $macro_name eq 'GOTO' ||
		 $macro_name eq 'TABLEJUMP');

    # Parameters which may be decoded by a separate decoding step go first:
    return $ORDER_PRE_ENCODE if (exists $ErjIG_Operands::DECODE_STEP_TEMPVAR{$param_type});

    # The rest go in the middle:
    return 5;
}


sub parse_and_build {
    my $cur_ins_class;
    my @cls_arg_names = ();
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
	    my %argmap = ErjIG_Util::reverse_map(@args);
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
    $enum_code .= ErjIG_Prefetching::enum_code($enum_count);
    $enum_code .= "\tpublic static final short MAX_OPCODE = OPC_PREFETCH_MAX;\n";
    $interp_code .= ErjIG_Prefetching::decoder_code();
    my $subst_map = {'ENCODE' => $encoder_code,
		     'ENUM' => $enum_code,
		     'ENCODER_HELPERS' => ErjIG_Prefetching::encoder_declaration_code(),
		     'INTERPRET' => $interp_code};
    writeFile("Interpreter.java", ErjIG_Util::multi_subst($encoder_template,$subst_map));
}

parse_and_build();
emit();
