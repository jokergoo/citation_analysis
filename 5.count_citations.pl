use strict;

print "reading pub_meta.tab...\n";
open F, "processed/pub_meta.tab" or die;

my $line = <F>;

my $pub = {};

my $paper_by_country = {};
my $paper_by_country_by_year = {};
my $paper_by_journal = {};
my $paper_by_journal_by_year = {};
my $n_paper = 0;

while($line = <F>) {
	$line =~s/\n//;
	my @line = split "\t", $line, -1;

	$pub->{$line[0]} = {journal_uid => $line[1],
		                pub_year => $line[2],
	                    n_authors => $line[3],
	                    country => $line[4]};

	$paper_by_country->{ $line[4] }->{ $line[0] } = 1;
	$paper_by_country_by_year->{ $line[4] }->{$line[2]}->{ $line[0] } = 1;
	$paper_by_journal->{ $line[1] }->{ $line[0] } = 1;
	$paper_by_journal_by_year->{ $line[1] }->{$line[2]}->{ $line[0] } = 1;

	$n_paper ++;
}
close F;


print "writing num_paper_country.tab...\n";
open OUT, ">processed/num_paper_country.tab" or die;
print OUT "country_cited\tpapers\n";
foreach my $x (sort keys %$paper_by_country) {
	print OUT "$x\t".(scalar keys %{$paper_by_country->{$x}})."\n";
}
close OUT;

print "writing num_paper_country_by_year.tab...\n";
open OUT, ">processed/num_paper_country_by_year.tab" or die;
print OUT "country_cited\tpub_year\tpapers\n";
foreach my $x (sort keys %$paper_by_country_by_year) {
	foreach my $y (sort keys %{$paper_by_country_by_year->{$x}}) {
		print OUT "$x\t$y\t".(scalar keys %{$paper_by_country_by_year->{$x}->{$y}})."\n";
	}
}
close OUT;

print "writing num_paper_journal.tab...\n";
open OUT, ">processed/num_paper_journal.tab" or die;
print OUT "journal\tpapers\n";
foreach my $x (sort keys %$paper_by_journal) {
	print OUT "$x\t".(scalar keys %{$paper_by_journal->{$x}})."\n";
}
close OUT;

print "writing num_paper_journal_by_year.tab...\n";
open OUT, ">processed/num_paper_journal_by_year.tab" or die;
print OUT "journal\tpub_year\tpapers\n";
foreach my $x (sort keys %$paper_by_journal_by_year) {
	foreach my $y (keys %{$paper_by_journal_by_year->{$x}}) {
		print OUT "$x\t$y\t".(scalar keys %{$paper_by_journal_by_year->{$x}->{$y}})."\n";
	}
}
close OUT;


#######



my $foo = `wc -l processed/citations.tab`;
$foo =~s/^[^0-9]*(\d+).*$/$1/;
print "reading citations.tab...\n";
open F, "processed/citations.tab" or die;

$line = <F>;

my $cite_by_country = {};
my $cite_by_country_by_year = {};
my $cite_by_country_by_year2 = {};
my $cite_by_country_by_year_both = {};
my $cite_by_journal = {};
my $cite_by_journal_by_year = {};
my $cite_by_journal_by_year_both = {};

my $paper_by_country = {};
my $paper_by_country_by_year = {};
my $paper_by_journal = {};
my $paper_by_journal_by_year = {};

my $i = 0;
while($line = <F>) {
	$line =~s/\n//;
	my @line = split "\t", $line, -1;

	$i ++;
	if($i % 10000 == 0) {
		print "$i/$output, ".$i/$output."\n"; 
	}

	# citing cited
	if(defined($pub->{$line[0]}) and defined($pub->{$line[1]})) {

		$cite_by_country->{ $pub->{$line[1]}->{country} }->{ $pub->{$line[0]}->{country} } ++;
		$cite_by_country_by_year->{ $pub->{$line[1]}->{country} }->{$pub->{$line[1]}->{pub_year}}->{ $pub->{$line[0]}->{country} } ++;
		$cite_by_country_by_year2->{ $pub->{$line[1]}->{country} }->{ $pub->{$line[0]}->{country} }->{$pub->{$line[0]}->{pub_year}} ++;
		$cite_by_country_by_year_both->{ $pub->{$line[1]}->{country} }->{$pub->{$line[1]}->{pub_year}}->{ $pub->{$line[0]}->{country} }->{$pub->{$line[0]}->{pub_year}} ++;
		
		$cite_by_journal->{ $pub->{$line[1]}->{journal_uid} }->{ $pub->{$line[0]}->{country} } ++;
		$cite_by_journal_by_year->{ $pub->{$line[1]}->{journal_uid} }->{$pub->{$line[1]}->{pub_year}}->{ $pub->{$line[0]}->{country} } ++;
		$cite_by_journal_by_year_both->{ $pub->{$line[1]}->{journal_uid} }->{$pub->{$line[1]}->{pub_year}}->{ $pub->{$line[0]}->{country} }->{$pub->{$line[0]}->{pub_year}} ++;

		$paper_by_country->{ $pub->{$line[1]}->{country} }->{ $line[1] } = 1;
		$paper_by_country_by_year->{ $pub->{$line[1]}->{country} }->{$pub->{$line[1]}->{pub_year}}->{ $line[1] } = 1;
		$paper_by_journal->{ $pub->{$line[1]}->{journal_uid} }->{ $line[1] } = 1;
		$paper_by_journal_by_year->{ $pub->{$line[1]}->{journal_uid} }->{$pub->{$line[1]}->{pub_year}}->{ $line[1] } = 1;

	} else {
		print "$line\n";  # should be no line printed
	}
}
close F;

print "writing num_cite_country_country.tab...\n";
open OUT, ">processed/num_cite_country_country.tab" or die;
print OUT "country_cited\tcountry_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_country) {
	foreach my $y (sort keys %{$cite_by_country->{$x}}) {
		print OUT "$x\t$y\t$cite_by_country->{$x}->{$y}\n";
	}
}
close OUT;

open OUT, ">processed/num_cite_country_country_by_year.tab" or die;
print OUT "country_cited\tpub_year_cited\tcountry_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_country_by_year) {
	foreach my $y (sort keys %{$cite_by_country_by_year->{$x}}) {
		foreach my $z (sort keys %{$cite_by_country_by_year->{$x}->{$y}}) {
			print OUT "$x\t$y\t$z\t$cite_by_country_by_year->{$x}->{$y}->{$z}\n";
		}
	}
}
close OUT;

open OUT, ">processed/num_cite_country_country_by_year2.tab" or die;
print OUT "country_cited\tcountry_citing\tpub_year_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_country_by_year2) {
	foreach my $y (sort keys %{$cite_by_country_by_year2->{$x}}) {
		foreach my $z (sort keys %{$cite_by_country_by_year2->{$x}->{$y}}) {
			print OUT "$x\t$y\t$z\t$cite_by_country_by_year2->{$x}->{$y}->{$z}\n";
		}
	}
}
close OUT;


open OUT, ">processed/num_cite_country_country_by_year_both.tab" or die;
print OUT "country_cited\tpub_year_cited\tcountry_citing\tpub_year_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_country_by_year_both) {
	foreach my $y (sort keys %{$cite_by_country_by_year_both->{$x}}) {
		foreach my $z (sort keys %{$cite_by_country_by_year_both->{$x}->{$y}}) {
			foreach my $k (sort keys %{$cite_by_country_by_year_both->{$x}->{$y}->{$z}}) {
				print OUT "$x\t$y\t$z\t$k\t$cite_by_country_by_year_both->{$x}->{$y}->{$z}->{$k}\n";
			}
		}
	}
}
close OUT;

# number of papers with at least citation
print "writing num_paper_cited_country.tab...\n";
open OUT, ">processed/num_paper_cited_country.tab" or die;
print OUT "country_cited\tpapers\n";
foreach my $x (sort keys %$paper_by_country) {
	print OUT "$x\t".(scalar keys %{$paper_by_country->{$x}})."\n";
}
close OUT;

print "writing num_paper_cited_country_by_year.tab...\n";
open OUT, ">processed/num_paper_cited_country_by_year.tab" or die;
print OUT "country_cited\tpub_year\tpapers\n";
foreach my $x (sort keys %$paper_by_country_by_year) {
	foreach my $y (sort keys %{$paper_by_country_by_year->{$x}}) {
		print OUT "$x\t$y\t".(scalar keys %{$paper_by_country_by_year->{$x}->{$y}})."\n";
	}
}
close OUT;





### journals
print "writing num_cite_journal_country.tab...\n";
open OUT, ">processed/num_cite_journal_country.tab" or die;
print OUT "journal_cited\tcountry_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_journal) {
	foreach my $y (sort keys %{$cite_by_journal->{$x}}) {
		print OUT "$x\t$y\t$cite_by_journal->{$x}->{$y}\n";
	}
}
close OUT;

print "writing num_cite_journal_country_by_year.tab...\n";
open OUT, ">processed/num_cite_journal_country_by_year.tab" or die;
print OUT "journal_cited\tpub_year\tcountry_citing\tcitations\n";
foreach my $x (sort keys %$cite_by_journal_by_year) {
	foreach my $y (sort keys %{$cite_by_journal_by_year->{$x}}) {
		foreach my $z (sort keys %{$cite_by_journal_by_year->{$x}->{$y}}) {
			print OUT "$x\t$y\t$z\t$cite_by_journal_by_year->{$x}->{$y}->{$z}\n";
		}
	}
}
close OUT;

print "writing num_cite_journal_country_by_year_both.tab...\n";
open OUT, ">processed/num_cite_journal_country_by_year_both.tab" or die;
print OUT "journal_cited\tpub_year\tcountry_citing\tciting_year\tcitations\n";
foreach my $x (sort keys %$cite_by_journal_by_year_both) {
	foreach my $y (sort keys %{$cite_by_journal_by_year_both->{$x}}) {
		foreach my $z (sort keys %{$cite_by_journal_by_year_both->{$x}->{$y}}) {
			foreach my $k (sort keys %{$cite_by_journal_by_year_both->{$x}->{$y}->{$z}}) {
				print OUT "$x\t$y\t$z\t$k\t$cite_by_journal_by_year_both->{$x}->{$y}->{$z}->{$k}\n";
			}
		}
	}
}
close OUT;


# number of papers with at least one citation
print "writing num_paper_cited_journal.tab...\n";
open OUT, ">processed/num_paper_cited_journal.tab" or die;
print OUT "journal\tpapers\n";
foreach my $x (sort keys %$paper_by_journal) {
	print OUT "$x\t".(scalar keys %{$paper_by_journal->{$x}})."\n";
}
close OUT;

print "writing num_paper_cited_journal_by_year.tab...\n";
open OUT, ">processed/num_paper_cited_journal_by_year.tab" or die;
print OUT "journal\tpub_year\tpapers\n";
foreach my $x (sort keys %$paper_by_journal_by_year) {
	foreach my $y (keys %{$paper_by_journal_by_year->{$x}}) {
		print OUT "$x\t$y\t".(scalar keys %{$paper_by_journal_by_year->{$x}->{$y}})."\n";
	}
}
close OUT;



