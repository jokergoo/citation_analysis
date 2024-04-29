use strict;

use Getopt::Long;

my $start = 1;
my $end = 1366;

GetOptions("start=i" => \$start,
	       "end=i" => \$end) or die;


## all analyses are restrected in wos core journals

# some ISSNs are not available in wos-core_SCIE 2023-December-18.csv
# script for fetching such ISSN mapping is in issn_mapping_missing.R
open F, "source/issn_missing.csv" or die;

my $issn_map = {};

while(my $line = <F>) {
	$line =~s/\n$//;
	my @line = split ",", $line, -1;
	$issn_map->{$line[0]} = $line[1];
	$issn_map->{$line[1]} = $line[0];
}
close F;


open F, "source/wos-core_SCIE 2023-December-18.csv" or die;

my $line = <F>;

my $wos_core_journals = {};

while($line = <F>) {
	$line =~s/\n$//;
	$line =~s/^"|"$//g;
	my @line = split "\",\"", $line, -1;
	if($line[5] ne "English") {  # only English journals
		next;
	}

	if($line[1] ne "") {
		$wos_core_journals->{$line[1]} = $line[6];
		if(defined($issn_map->{$line[1]})) {
			$wos_core_journals->{ $issn_map->{$line[1]} } = $line[6];
		}
	}
	if($line[2] ne "") {
		$wos_core_journals->{$line[2]} = $line[6];
		if(defined($issn_map->{$line[2]})) {
			$wos_core_journals->{ $issn_map->{$line[2]} } = $line[6];
		}
	}
}
close F;


my $journal = {};
my $pub = {};
my $pub_other = {};
my $cite = {};

my $file;
my $count = {
	"no_wos" => 0,
};
for(my $i = $start; $i <= $end; $i ++) {
	if($i < 10) {
		$file = "pubmed24n000$i.normalized.tab";
	} elsif($i < 100) {
		$file = "pubmed24n00$i.normalized.tab";
	} elsif($i < 1000) {
		$file = "pubmed24n0$i.normalized.tab";
	} else {
		$file = "pubmed24n$i.normalized.tab";
	}

	print "$file\n";

	open F, "/Volumes/WD/pubmed_formatted/$file" or die;

	my $line = <F>;

	while($line = <F>) {
		$line =~s/\n$//;
		my @line = split "\t", $line, -1;

		my @issn = split ",", $line[3];
		my $issn_flag = 0;
		foreach my $is (@issn) {
			if(defined($wos_core_journals->{$is})) {  # in wos core collection
				$issn_flag = 1;
				next;
			}
		}
		if(!$issn_flag) {
			$count->{no_wos} ++;
			next;
		}
		
		if(!defined($journal->{$line[5]})) {
			$journal->{$line[5]} = {"journal" => $line[1],
			                        "journal_abbr" => $line[2],
			                        "journal_issn" => $line[3],
			                        "journal_country" => $line[4]};
		}

		# citations
		my @ref = split ";", $line[10], -1;
		foreach my $x (@ref) {
			$cite->{$line[0]}->{$x} = 1;
		}

		my $c = determine_country($line[11]);

		if($c !~/^_/) {
			# papers are restrctedin wos core collection and have clear country assigned
			$pub->{$line[0]} = {"journal_uid" => $line[5],
					            "pub_year" => $line[7],
				                "n_authors" => $line[8],
				                "country" => $c,
				                "country_type" => "_domestic_",
				                "file_id" => $i,
				                "n_references" => scalar @ref
				            };
			$count->{"_domestic_"} ++;
		} else {
			# print "$line[0]\t$line[7]\n";  # there is no dominant country for the paper
			$pub_other->{$line[0]} = {"journal_uid" => $line[5],
					            "pub_year" => $line[7],
				                "n_authors" => $line[8],
				                "country" => $line[11],
				                "country_type" => $c,
				                "file_id" => $i,
				                "n_references" => scalar @ref
				            };
			$count->{$c} ++;
		}
		
		
		
	}
	close F;
}

open STAT, ">processed/paper_stat2.txt" or die;
foreach my $x (sort keys %$count) {
	print STAT "$x\t$count->{$x}\n";
}
close STAT;


## $pub contains papers
#   - published between 2000 ~ 2023
#   - has unique country identified
#   - in English
#   - <= 50 authors
#   - wos journal core collection
#
## $pub_other is similar as $pub, except
#   - has multiple country or has no country information


# this function return domestic

## a country is assigned to a paper if
## - only one author
## - if the first and the last author have countires assigned, the two countries should be the same
## - the major country should be > 80% in the author list

use List::Vectorize;
sub determine_country {
	my $country = shift;

	if($country =~/^\s*$/) {
		return "_empty_";
	}

	my $c1 = [ split ";", $country, -1 ];
	my $c2 = [];
	for(my $i = 0; $i <= $#$c1; $i ++) {
		if($c1->[$i] eq "") {
			$c2->[$i] = [];
		} else {
			$c2->[$i] = [ split '\|', $c1->[$i], -1 ];
			$c2->[$i] = setdiff($c2->[$i], [""]);
		}
	}


	# first author should not be in more than one country, but the value can be zero
	if(n_unique($c2->[0]) > 1) {
		return "_international_";
	}

	# last author can only be in one single country
	if(n_unique($c2->[$#$c2]) != 1) {
		return "_international_";
	}

	my $major_country = $c2->[$#$c2]->[0];

	# for each of other authors, if he/she has multiple 
	# countries, check whether he/she also be in the major country
	# If yes, the major country is selected as his/her country

	my $unique = [];
	for(my $i = 0; $i <= $#$c2; $i ++) {
		if(len($c2->[$i]) == 0) {
			$unique->[$i] = "";
		} else {
			if(is_element($major_country, $c2->[$i])) {
				$unique->[$i] = $major_country;
			} else {
				$unique->[$i] = "_other_country_";
			}
		}
	}


	my $tb = freq($unique);
	if(defined($tb->{""})) {
		delete($tb->{""});
	}

	my $n = scalar keys %$tb;
	if($n == 0) {   # no country information
		return "_empty_";
	} elsif($n == 1) {  # only one country
		return $major_country;
	} else {
		if($unique->[0] ne "" and $unique->[$#$unique] ne "") {
			if($unique->[0] ne $unique->[$#$unique]) {  # first country should be identical to the last country
				return "_international_";
			}
		}

		my $max = which_max_hash($tb);
		if($tb->{$max}/sum([values %$tb]) >= 0.8) {
			if($max eq $major_country) {
				return $major_country
			} else {
				return "_international_";
			}
		} else {
			return "_international_";
		}
	}
}

# the key for the maximal value
sub which_max_hash {
	my $hash = shift;
	my $x = undef;
	my $v = undef;

	foreach my $k (keys %$hash) {
		if(!defined($x)) {
			$x = $k;
			$v = $hash->{$k};
		} elsif($hash->{$k} > $v) {
			$v = $hash->{$k};
			$x = $k;
		}
	}

	return $x;
}

sub n_unique {
	my $array = shift;
	if(defined($array)) {
		return len(unique($array));
	} else {
		return 0;
	}
}


print "journal_meta.tab\n";
open OUT, ">processed/journal_meta.tab" or die;
print OUT "journal_uid\tjournal\tjournal_abbr\tjournal_issn\tjournal_country\n";
foreach my $x (sort keys %$journal) {
	print OUT "$x\t$journal->{$x}->{journal}\t$journal->{$x}->{journal_abbr}\t$journal->{$x}->{journal_issn}\t$journal->{$x}->{journal_country}\n";
}
close OUT;


print "pub_meta.tab\n";
open OUT, ">processed/pub_meta.tab" or die;
print OUT "pmid\tjournal_uid\tpub_year\tn_authors\tcountry\tcountry_type\tfile_id\tn_references\n";
foreach my $x (sort keys %$pub) {
	print OUT "$x\t$pub->{$x}->{journal_uid}\t$pub->{$x}->{pub_year}\t$pub->{$x}->{n_authors}\t$pub->{$x}->{country}\t$pub->{$x}->{country_type}\t$pub->{$x}->{file_id}\t$pub->{$x}->{n_references}\n";
}
close OUT;

# # international publications
# print "pub_other_meta.tab\n";
# open OUT, ">processed/pub_other_meta.tab" or die;
# print OUT "pmid\tjournal_uid\tpub_year\tn_authors\tcountry\tcountry_type\tfile_id\tn_references\n";
# foreach my $x (sort keys %$pub_other) {
# 	print OUT "$x\t$pub_other->{$x}->{journal_uid}\t$pub_other->{$x}->{pub_year}\t$pub_other->{$x}->{n_authors}\t$pub_other->{$x}->{country}\t$pub_other->{$x}->{country_type}\t$pub_other->{$x}->{file_id}\t$pub_other->{$x}->{n_references}\n";
# }
# close OUT;


# includes all citations for papers between 2000-2023
print "citations.tab\n";
open OUT, ">processed/citations.tab" or die;
print OUT "citing\tcited\n";
foreach my $x (sort keys %$cite) {
	if(!defined($pub->{$x})) {  # only wos core journals is included in $pub
		next;
	}
	foreach my $y (sort keys %{$cite->{$x}}) {
		if(!defined($pub->{$y})) {
			next;
		}
		print OUT "$x\t$y\n";
	}
}
close OUT;

exit 0;


# also contain international publications
print "citations_full.tab\n";
open OUT, ">processed/citations_full.tab" or die;
print OUT "citing\tcited\n";
foreach my $x (sort keys %$cite) {
	if(!defined($pub->{$x}) and !defined($pub_other->{$x})) {  # only wos core journals is included in $pub
		next;
	}
	foreach my $y (sort keys %{$cite->{$x}}) {
		if(!defined($pub->{$y}) and !defined($pub_other->{$y})) {
			next;
		}
		print OUT "$x\t$y\n";
	}
}
close OUT;



##############################
## impact factors, only between papers dominant in single countries
##############################

# papers that have no citation
my $has_citation = {};
foreach my $x (sort keys %$cite) {
	if(!defined($pub->{$x})) {  # only wos core journals is included in $pub
		next;
	}
	foreach my $y (sort keys %{$cite->{$x}}) {
		if(!defined($pub->{$y})) {
			next;
		}
		$has_citation->{$y} = 1;
	}
}
my $has_no_citation = {};
foreach my $x (keys %$pub) {
	if(!defined($has_citation->{$x})) {
		$has_no_citation->{$x} = 1;
	}
}

my $IF;

# all citations
$IF = calc_IF("country", $cite, $pub);
output_IF($IF, "IF_by_country.txt");

# citations from the same country
$IF = calc_IF("country", $cite, $pub, sub { $pub->{$_[0]}->{"country"} eq $pub->{$_[1]}->{"country"} });
output_IF($IF, "IF_by_country_domistic.txt");


# international citations
$IF = calc_IF("country", $cite, $pub, sub { $pub->{$_[0]}->{"country"} ne $pub->{$_[1]}->{"country"} });
output_IF($IF, "IF_by_country_international.txt");

# citations only from US
$IF = calc_IF("country", $cite, $pub, sub { $pub->{$_[1]}->{"country"} eq "United States"; });
output_IF($IF, "IF_by_country_only_from_US.txt");

$IF = calc_IF("country", $cite, $pub, sub { $pub->{$_[1]}->{"country"} eq "China"; });
output_IF($IF, "IF_by_country_only_from_China.txt");


# IF by journal
$IF = calc_IF("journal_uid", $cite, $pub);
output_IF($IF, "IF_by_journal.txt");


# general IF:
# citation in year y, published in year x1-x2
sub calc_IF {
	my $type = shift;
	my $cite = shift;
	my $pub = shift;
	my $restrict_fun = shift;

	no warnings;

	if(!defined($restrict_fun)) {
		$restrict_fun = sub {1;}
	}
	
	my $nc = {};
	my $np = {};

	my $i = 0;
	foreach my $x (keys %$cite) {
		if(!defined($pub->{$x})) {
			next;
		}
		foreach my $y (keys %{$cite->{$x}}) {
			if(!defined($pub->{$y})) {
				next;
			}

			$i ++;
			if($i % 10000 == 0) {
				print "$i\n"; 
			}

			my $c = $pub->{$y}->{$type};
			if($restrict_fun->($y, $x)) {
				
				if($pub->{$x}->{"pub_year"} - $pub->{$y}->{"pub_year"} == 1 or $pub->{$x}->{"pub_year"} - $pub->{$y}->{"pub_year"} == 2) {
					$nc->{$c}->{$pub->{$x}->{"pub_year"}} ++;
					$np->{$c}->{$pub->{$x}->{"pub_year"}}->{$y} = 1;
				}

			}
		}
	}

	foreach my $y (keys %$has_no_citation) {
		my $c = $pub->{$y}->{$type};
		if($pub->{$y}->{"pub_year"}+1 <= 2023) {
			$np->{$c}->{$pub->{$y}->{"pub_year"}+1}->{$y} = 1;
		}
		if($pub->{$y}->{"pub_year"}+2 <= 2023) {
			$np->{$c}->{$pub->{$y}->{"pub_year"}+2}->{$y} = 1;
		}
	}

	my $IF = {};
	foreach my $c (keys %$nc) {
		foreach my $y (keys %{$nc->{$c}}) {
			$IF->{$c}->{$y} = $nc->{$c}->{$y}/(scalar keys %{$np->{$c}->{$y}});
		}
	}

	return $IF;
}

sub output_IF {
	my $IF = shift;
	my $output = shift;

	open OUT, ">processed/$output" or die;

	foreach my $c (sort keys %$IF) {
		foreach my $y (sort keys %{$IF->{$c}}) {
			print OUT "$c\t$y\t$IF->{$c}->{$y}\n";
		}
	}
	close OUT;
}

