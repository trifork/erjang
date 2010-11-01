package ErjIG_Operands;
# Erjang Interpreter Generator - Operands metadata and function

our %EXPORT_TAGS = ( 'all' => [ qw(
base_types
basetype_test_exp
basetype_encoding_exp
basetype_decoding_exp
class_repr_type
) ] );

use ErjIG_Util;

# Static data:
our %TYPES_SUBTYPES =
    (
     'S' => ['c', 'x', 'y'],
     'D' => ['x', 'y'],
     'SF' => ['c', 'x', 'y', 'f'],
     'DF' => ['x', 'y', 'f'],
     'A' => ['c'],
     'L0' => ['L', 'nolabel'], # Label, possibly nofail
     'EG' => ['G', 'E']        # Ext. fun, possibly a guard
     );
our %TYPES_OPERAND_CLASS =
    (
     # Abstract:
     'S' => "Operands.SourceOperand",
     'D' => "Operands.DestinationOperand",
     # Concrete:
     'x' => "Operands.XReg",
     'y' => "Operands.YReg",
     'f' => "Operands.FReg",
     'c' => "Operands.Literal",
     'I' => "int",
     'IL' => "int", # Integer-as-label
     'L' => "Operands.Label",
     'nolabel' => ["Operands.Label", "(# == null || #.nr == 0)"],
     'E' => "ExtFun",
     'G' => ["ExtFun", "# instanceof ExtFun && \$onFail != null"],
     'JV' => "Operands.SelectList",
     'JA' => "Operands.SelectList"
     );
our %TYPES_DECODE =
    (
     'c' => "consts[#]",
     'x' => "reg[#]",
     'y' => "stack[sp - (#)]",
     'f' => "freg[#]",
     'I' => "(short) (#)",	#  Signed integer.
     'IL' => "(#)",
     'L' => "(#)",
     'nolabel' => "nofailLabel()",
     'E' => "ext_funs[#]",
     'G' => "ext_funs[#]",
     'JV' => "value_jump_tables[#]",
     'JA' => "arity_jump_tables[#]"
     );
our %TYPES_ENCODE =
(
 'c' => "encodeLiteral(#)",
 'x' => "#.nr",
 'y' => "#.nr",
 'f' => "#.nr",
 'I' => "(char)(#)",		# Signed integer. - TODO: Range check
 'IL' => "encodeLabel(#)",
 'L' => "encodeLabel(#.nr)",
 'E' => "encodeExtFun(#)",
 'G' => "encodeGuardExtFun(#)",
 'JV' => "encodeValueJumpTable(#)",
 'JA' => "encodeArityJumpTable(#)"
 );
our %DECODE_STEP_TEMPVAR = (
    'S' => 'term'
    );
our %TYPES_ALLOWED_OPS =
    (
     'x' => {'GET'=>1, 'SET'=>1},
     'y' => {'GET'=>1, 'SET'=>1},
     'f' => {'GET'=>1, 'SET'=>1, 'FGET'=>1, 'FSET'=>1},
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

our %PRIMITIVE_TYPES = ('I' => 1, 'IL'=>1);

sub base_types($$);
sub base_types($$) {
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

sub basetype_test_exp($$) {
    my ($base_type, $src_exp) = @_;

    my $opClass = $TYPES_OPERAND_CLASS{$base_type};
    my $test_exp;
    if (ref($opClass) eq 'ARRAY') { # Special test, beyond instanceof
	my @test_spec = @{$opClass};
	$opClass = $test_spec[0];
	$test_exp = $test_spec[1];
	$test_exp =~ s/\#/($src_exp)/g;
    } elsif (exists $PRIMITIVE_TYPES{$base_type}) {
	$test_exp = "true"
    } else {
	$test_exp = "($src_exp) instanceof $opClass";
    }

    return ($test_exp, $opClass);
}

sub basetype_encoding_exp($$) {
    my ($base_type, $src_exp) = @_;
    return "" if (! exists $TYPES_ENCODE{$base_type});
    my $encoding_exp_code = ErjIG_Util::subst($TYPES_ENCODE{$base_type}, $src_exp);
    return $encoding_exp_code;
}

sub basetype_decoding_exp($$) {
    my ($base_type, $encoded_exp) = @_;
    return ErjIG_Util::subst($TYPES_DECODE{$base_type}, $encoded_exp);
}

sub class_repr_type($ ) {
    my ($base_type) = @_;
    return $TYPES_OPERAND_CLASS{$base_type};
}

1; # OK
