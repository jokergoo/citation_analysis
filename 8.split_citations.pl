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


my @all_countries = ("Argentina","Australia","Austria","Bangladesh","Belgium","Brazil","Bulgaria","Canada","Chile","China","Colombia","Croatia","Czech Republic","Denmark","Egypt","Estonia","Ethiopia","Finland","France","Germany","Ghana","Greece","Hong Kong","Hungary","India","Indonesia","Iran","Ireland","Israel","Italy","Japan","Jordan","Kenya","Kuwait","Lebanon","Lithuania","Luxembourg","Malaysia","Mexico","Morocco","Netherlands","New Zealand","Nigeria","Norway","Pakistan","Peru","Poland","Portugal","Qatar","Romania","Russia","Saudi Arabia","Serbia","Singapore","Slovakia","Slovenia","South Africa","South Korea","Spain","Sri Lanka","Sweden","Switzerland","Taiwan","Tanzania","Thailand","Tunisia","Turkey","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Vietnam");
my $all_countries = {};

foreach my $c (@all_countries) {
	$all_countries->{$c} = 1;
}

foreach my $c (@all_countries) {
	print "reading citations.tab...\n";
	open F, "processed/citations.tab" or die;


	$line = <F>;
	my $fh = {};

	my $i = 0;
	while($line = <F>) {
		$line =~s/\n//;
		my @line = split "\t", $line, -1;

		$i ++;
		if($i % 10000 == 0) {
			print "$c: $i\n"; 
		}

		if(defined($pub->{$line[0]}) and defined($pub->{$line[1]})) {

			if(!defined($all_countries->{$pub->{$line[0]}->{country}})) {
				next;
			}
			if(!defined($all_countries->{$pub->{$line[1]}->{country}})) {
				next;
			}

			if($pub->{$line[1]}->{country} ne $c) {
				next;
			}

			if($pub->{$line[1]}->{country} ne $pub->{$line[0]}->{country}) {
				next;
			}


			my $nm = "$pub->{$line[1]}->{country} -> $pub->{$line[0]}->{country}";
			if(!defined($fh->{$nm})) {

				open $fh->{$nm}, ">processed/citations_split/citations_$pub->{$line[1]}->{country}_to_$pub->{$line[0]}->{country}.tab" or die "citations_$pub->{$line[1]}->{country}_to_$pub->{$line[0]}->{country}.tab";
			}


			print {$fh->{$nm}} "$line\n";
		}
	}
	close F;

	foreach my $x (values %$fh) {
		close $x;
	}
}


