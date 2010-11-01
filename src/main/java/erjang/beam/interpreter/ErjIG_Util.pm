package ErjIG_Util;
# Erjang Interpreter Generator - Utility functions

our %EXPORT_TAGS = ( 'all' => [ qw(
indent
reverse_map
cartesian_product
subst
multi_subst
) ] );

sub indent($) {
    ${$_[0]} =~ s/^/\t/gm; # Indent with one level
}

sub reverse_map {
    my @a = @_;
    my %res = ();
    for (my $i=0; $i<=$#a; $i++) {$res{$a[$i]} = $i;}
    return %res;
}

sub cartesian_product {
    my @lists = @_;
    my @res = ([]);
    foreach my $list (@lists) {
	@res = map {my $elm=$_; map {[@{$_}, $elm]} @res} @{$list};
    }
    return @res;
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

my $BALANCED_RE = '(?:(?(DEFINE)(?<BALANCED>([^()]+|\((?&BALANCED)\))*))([^()]+|\((?&BALANCED)\))+)';
my $BALANCED_NO_TOPLEVEL_COMMA_RE = '(?:(?(DEFINE)(?<BALANCED>([^()]+|\((?&BALANCED)\))*))([^(),]+|\((?&BALANCED)\))+)';
sub back_subst {
    my ($subject, $subst_map) = @_;
    foreach my $macro (keys %{$subst_map}) {
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
die "Wrong number of arguments to macro '$macro' (expected ".scalar @formals.", found ".scalar @actuals.": ".join(',',@actuals).")" unless ($#actuals==$#formals);
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
    pos($s) = undef;
    while ($s =~ /\G($BALANCED_NO_TOPLEVEL_COMMA_RE)(,|$|(?=\)))/g) {push(@res,$1);}
    return @res;
}

sub make_map {
    my ($key_list, $value_list) = @_;
    my %res = ();
    my $last_index = scalar(@{$key_list}-1);
    for my $i (0..$last_index) {$res{$key_list->[$i]} = [$value_list->[$i]];}
    return \%res;
}
