package ErjIG_Prefetching;
# Erjang Interpreter Generator - Code generators for prefetch helper functions

our %EXPORT_TAGS = ( 'all' => [ qw(
ensureGenerated_encodePrefetch
ensureGenerated_encodePolymorphParameter
generated_code
) ] );

use ErjIG_Metadata;

my @generated_code = "";
my %generated = ();
my $prefetch_enum_offset = 0;

sub generated_code() {return join("\n", @generated_code);}

sub ensureGenerated_encodePrefetch {
    my (@arg_class_seq) = @_;

## Sample generated code:
# private int encodePrefetch_S_S_S(S v1, S v2, S v3) {
#   int x1 = encodePolymorphParameter_S(v1);
#   int x2 = encodePolymorphParameter_S(v2);
#   int x3 = encodePolymorphParameter_S(v3);
#   return OPC_FETCH_S_S_S + (((x1) * SIZE_FETCH_S + x2) * SIZE_FETCH_S) + x3;
# }

    my $combi_suffix = join('_', @arg_class_seq);
    my $method_name = "encodePrefetch_$combi_suffix";
    return $method_name if (exists $generated{$method_name}); # Already generated.
    $generated{$method_name} = 1;

    my @arg_list = ();
    my @encode_lines = ();
    my $opc_exp = "0";
    my $combi_size = 1;
    my $i = 0;
    for my $arg_class (@arg_class_seq) {
	$i++;
	ensureGenerated_encodePolymorphParameter($arg_class);
	push(@arg_list, "$arg_class v$i");
	my ($encodeOneMethodName, $variantCountName, $variantCount) =
	    ensureGenerated_encodePolymorphParameter($arg_class);
	push(@encode_lines, "\tint x$i = $encodeOneMethodName(v$i);\n");
	$opc_exp = "($opc_exp * $variantCountName + x$i)";
	$combi_size *= $variantCount;
    }
    my $opc_base_name = "OPC_FETCH_$combi_suffix";

    my $method = "private int $method_name(".join(", ", @arg_list).")\n".
	join("",@encode_lines).
	"\treturn $opc_base_name + $opc_exp;\n".
	"}\n";

    my $opc_base_def = "private final int $opc_base_name = ".
	"OPC_PREFETCH_BASE + ".$prefetch_enum_offset.
	"; // and $combi_size forth";
    $prefetch_enum_offset += $combi_size;
    push(@generated_code, $opc_base_def);
    push(@generated_code, $method);

    return $method_name;
}

sub ensureGenerated_encodePolymorphParameter {
    my ($arg_class) = @_;


## Sample generated code:
# private int encodePolymorphParameter_S(S v) {
#   switch (v...) {
#     case X: emit(((X)v).nr()); return 0;
#     case Y: emit(((Y)v).nr()); return 1;
#     case C: emit(((C)v).nr()); return 2;
#     default: throw(...);
#   }
# }
    my $suffix = "_$arg_class";
    my $method_name = "encodePolymorphParameter$suffix";
    my $size_name = "SIZE_FETCH$suffix";
    unless (exists $generated{$method_name}) { # Already generated?
	my $body = "throw new Error(\"Unrecognized operand: \"+v);\n";
	my @base_types = ErjIG_Metadata::base_types($arg_class, 'GET');
	my $size_value = scalar(@base_types);
	foreach my $base_type (reverse @base_types) {
	    my ($type_test_exp, $opClass) = ErjIG_Metadata::basetype_test_exp($base_type, "v");
	    $body = "if ($type_test_exp) {\n".
		"\t$opClass w = ($opClass)v;\n".
		"\temit(".ErjIG_Metadata::basetype_encoding_exp($base_type, "w").");\n".
		"} else ".$body;
	}

	ErjIG_Util::indent(\$body);
	my $method = "private int $method_name($arg_class v) {\n".
	    $body.
	    "}\n";

	my $size_def = "private final int $size_name = $size_value;";
	push(@generated_code, $size_def);
	push(@generated_code, $method);

	$generated{$method_name} = $size_value;
    }
    my $size_value = $generated{$method_name};
    return ($method_name, $size_name, $size_value);
}

1; # OK
