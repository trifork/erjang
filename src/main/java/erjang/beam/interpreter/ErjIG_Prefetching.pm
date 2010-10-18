package ErjIG_Prefetching;
# Erjang Interpreter Generator - Code generators for prefetch helper functions

our %EXPORT_TAGS = ( 'all' => [ qw(
ensureGenerated_encodePrefetch
ensureGenerated_encodePolymorphParameter
enum_code
encoder_helper_code
decoder_code
) ] );

use ErjIG_Operands;

my @enum_code = ();
my @encoder_declaration_code = ();
my @decoder_code = ();
my %generated = ();
my $prefetch_enum_offset = 0;

sub enum_code($) {
    my ($enum_start) = @_;
    unshift(@enum_code, "private static final int OPC_PREFETCH_BASE = $enum_start;");
    push(@enum_code, "public static final short OPC_PREFETCH_MAX = OPC_PREFETCH_BASE + $prefetch_enum_offset;");
    return join("\n", @enum_code);
}

sub encoder_declaration_code() {
    return "//---------- Prefetch-related declarations:\n".
	join("\n", @encoder_declaration_code);
}

sub decoder_code() {
    return "//---------- Prefetch decoding:\n".
	join("\n", @decoder_code);
}

sub ensureGenerated_encodePrefetch {
    my (@arg_class_seq) = @_;

##### Sample generated code:
# private void encodePrefetch_S_S_S(S v1, S v2, S v3) {
#   int x1 = encodePolymorphParameter_S(v1);
#   int x2 = encodePolymorphParameter_S(v2);
#   int x3 = encodePolymorphParameter_S(v3);
#   return OPC_FETCH_S_S_S + (((x1) * SIZE_FETCH_S + x2) * SIZE_FETCH_S) + x3;
# }
#
##### Corresponding decoder code:
# case OPC_FETCH_S_S_S + (((S_VARIANT_x) * SIZE_FETCH_S + S_CASE_y) * SIZE_FETCH_S) + S_CASE_c: {
#     int _prefetched1 = code[pc++];
#     int _prefetched2 = code[pc++];
#     int _prefetched3 = code[pc++];
#     prefetched1 = reg[_prefetched1];
#     prefetched2 = stack[sp - (prefetched2)];
#     prefetched3 = consts[_prefetched3];
#   } break;
#####

    my $combi_suffix = join('_', @arg_class_seq);
    my $encoder_method_name = "encodePrefetch_$combi_suffix";
    return $encoder_method_name if (exists $generated{$encoder_method_name}); # Already generated.
    $generated{$encoder_method_name} = 1;

    #==== Encoder code:
    my @arg_list = ();
    my @encode_lines = ();
    my $opc_exp = "0";
    my $combi_size = 1;
    my $i = 0;
    for my $arg_class (@arg_class_seq) {
	$i++;
	ensureGenerated_encodePolymorphParameter($arg_class);
	my $arg_repr_type = ErjIG_Operands::class_repr_type($arg_class);
	push(@arg_list, "$arg_repr_type v$i");
	my ($encodeOneMethodName, $variantCountName, $variantCount) =
	    ensureGenerated_encodePolymorphParameter($arg_class);
	push(@encode_lines, "\tint x$i = $encodeOneMethodName(v$i);\n");
	$opc_exp = "($opc_exp * $variantCountName + x$i)";
	$combi_size *= $variantCount;
    }
    my $opc_base_name = "OPC_FETCH_$combi_suffix";

    my $method = "private int $encoder_method_name(".join(", ", @arg_list).") {\n".
	join("",@encode_lines).
	"\treturn $opc_base_name + $opc_exp;\n".
	"}\n";

    my $opc_base_def = "private static final int $opc_base_name = ".
	"OPC_PREFETCH_BASE + ".$prefetch_enum_offset.
	"; // and $combi_size forth";
    $prefetch_enum_offset += $combi_size;
    push(@enum_code,        $opc_base_def);
    push(@encoder_declaration_code, $method);

    #==== Decoder code:
    my @base_type_lists =
	map {[ErjIG_Operands::base_types($_, 'GET')]} @arg_class_seq;
    my @base_type_combinations =
	ErjIG_Util::cartesian_product(@base_type_lists);

    for my $combination (@base_type_combinations) {
	my $opc_exp = "0";
	my @code1 = ();
	my @code2 = ();
	my $i=0;
	foreach my $base_type (@{$combination}) {
	    my $nr = ++$i;
	    my $class = $arg_class_seq[$nr-1];
	    my $variantCountName = "SIZE_FETCH_$class";
	    my $variantName = "${class}_VARIANT_${base_type}";
	    $opc_exp = "($opc_exp * $variantCountName + $variantName)";
	    push(@code1, "int _prefetched$nr = code[pc++];");
	    push(@code2, "prefetched$nr = ".ErjIG_Operands::basetype_decoding_exp($base_type, "_prefetched$nr").";");
	}

	push(@decoder_code,
	     "case $opc_base_name + $opc_exp: {\n".
	     join("\n",@code1)."\n".
	     join("\n",@code2)."\n".
	     "} break;");
    }

    return $encoder_method_name;
}

sub ensureGenerated_encodePolymorphParameter {
    my ($arg_class) = @_;


## Sample generated code:
# private int encodePolymorphParameter_S(S v) {
#   if (<v in opclass X>) {
#     emit(((X)v).nr()); return S_VARIANT_x;
#   } else if (<v in opclass Y>) {
#     case Y: emit(((Y)v).nr()); return S_VARIANT_y;
#   } else {
#     default: throw(...);
#   }
# }
    my $suffix = "_$arg_class";
    my $method_name = "encodePolymorphParameter$suffix";
    my $size_name = "SIZE_FETCH$suffix";
    unless (exists $generated{$method_name}) { # Already generated?
	my $body = "throw new Error(\"Unrecognized operand: \"+v);\n";
	my @base_types = ErjIG_Operands::base_types($arg_class, 'GET');
	my $size_value = scalar(@base_types);
	my $variantCounter = 0;
	foreach my $base_type (reverse @base_types) {
	    my ($type_test_exp, $opClass) = ErjIG_Operands::basetype_test_exp($base_type, "v");
	    my $variantNumber = $variantCounter++;
	    my $variantName = "${arg_class}_VARIANT_$base_type";
	    push(@enum_code, "public static final int $variantName = $variantNumber;");
	    $body = "if ($type_test_exp) {\n".
		"\t$opClass w = ($opClass)v;\n".
		"\temit(".ErjIG_Operands::basetype_encoding_exp($base_type, "w").");\n".
		"\treturn $variantName;\n".
		"} else ".$body;
	}

	ErjIG_Util::indent(\$body);
	my $arg_repr_type = ErjIG_Operands::class_repr_type($arg_class);
	my $method = "private int $method_name($arg_repr_type v) {\n".
	    $body.
	    "}\n";

	my $size_def = "private static final int $size_name = $size_value;";
	push(@enum_code, $size_def);
	push(@encoder_declaration_code, $method);

	$generated{$method_name} = $size_value;
    }
    my $size_value = $generated{$method_name};
    return ($method_name, $size_name, $size_value);
}

1; # OK
