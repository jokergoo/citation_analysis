use strict;

print "reading pub_meta.tab...\n";
open F, "processed/pub_meta.tab" or die;

my $line = <F>;

my $pub = {};

while($line = <F>) {
	$line =~s/\n//;
	my @line = split "\t", $line, -1;

	$pub->{$line[0]} = {journal_uid => $line[1],
		                pub_year => $line[2],
	                    n_authors => $line[3],
	                    country => $line[4]};
}
close F;


print "reading citations.tab...\n";
open F, "processed/citations.tab" or die;

my $paper_sets = {};

$line = <F>;
my $i = 0;
while($line = <F>) {
	$line =~s/\n//;
	my @line = split "\t", $line, -1;

	$i ++;
	if($i % 10000 == 0) {
		print "$i\n"; 
	}

	# citing cited
	if(defined($pub->{$line[0]}) and defined($pub->{$line[1]})) {

		my $nm = $pub->{$line[1]}->{country}." -> ".$pub->{$line[0]}->{country};
		$paper_sets->{$line[0]}->{$nm} = 1;
		$paper_sets->{$line[1]}->{$nm} = 1;

	} 
}
close F;



print "reading pmid_to_mesh.tab...\n";
open F, "processed/pmid_to_mesh.tab" or die;

my $mesh = {};
$i = 0;

while($line = <F>) {
	$line =~s/\n//;
	my @line = split "\t", $line, -1;

	$i ++;
	if($i % 10000 == 0) {
		print "$i\n"; 
	}

	if(!defined($paper_sets->{$line[0]})) {
		next;
	}

	if($line[1] eq "") {
		next;
	}

	my @terms = split ";", $line[1];
	foreach my $t (@terms) {
		$mesh->{$line[0]}->{$t} = 1;
	}
}
close F;


foreach my $x (keys %$paper_sets) {
	if(!defined($mesh->{$x})) {
		delete($paper_sets->{$x});
	}
}


scalar keys %$paper_sets == scalar keys %$mesh;

my $paper_set_size = {};
foreach my $id (keys %$paper_sets) {
	foreach my $nm (keys %{$paper_sets->{$id}}) {
		$paper_set_size->{$nm} ++;
	}
}


my $mesh_size = {};
foreach my $id (keys %$mesh) {
	foreach my $t (keys %{$mesh->{$id}}) {
		$mesh_size->{$t} ++;
	}
}




my $N = scalar keys %$paper_sets;

my $k = {};

$i = 0;
foreach my $id (keys %$paper_sets) {

	$i ++;
	if($i % 10000 == 0) {
		print "$i/$N, ". $i/$N. "\n"; 
	}

	foreach my $x (keys %{$paper_sets->{$id}}) {
		next if($paper_set_size->{$x} < 50);
		foreach my $y (keys %{$mesh->{$id}}) {
			next if($mesh_size->{$y} < 50);
			$k->{$x}->{$y} ++;
		}
	}
}


use Math::GSL::CDF qw/:all/;


open OUT, ">processed/mesh_enrichment.tab" or die;
print OUT "paper_set\tmesh_term\tk\tmesh_size\tpaper_size\ttotal\tp_value\n";

foreach my $nm (keys %$k) {
	foreach my $t (keys %{$k->{$nm}}) {
		next if($k->{$nm}->{$t} <= 50);
		print OUT "$nm\t$t\t$k->{$nm}->{$t}\t$mesh_size->{$t}\t$paper_set_size->{$nm}\t$N\t";
		print OUT 1 - gsl_cdf_hypergeometric_P($k->{$nm}->{$t}-1, $mesh_size->{$t}, $N - $mesh_size->{$t}, $paper_set_size->{$nm});
		print OUT "\n";
	}
}

close OUT;



