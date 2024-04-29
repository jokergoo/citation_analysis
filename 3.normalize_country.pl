use strict;

use Getopt::Long;
use List::Vectorize;
use Data::Dumper;


my $start = 1;
my $end = 1366;


my $dict = {};

GetOptions("start=i" => \$start,
	       "end=i" => \$end) or die;


my $official = {};
my $domain = {};
my $country_code = {};
my $country_code_cca3 = {};

# words that contain country information, map to standard country names
$official->{"Deutschland"} = "Germany";
$official->{"Österreich"} = "Austria";
$official->{"Osterreich"} = "Austria";
$official->{"Schweiz"} = "Switzerland";
$official->{"Swiss"} = "Switzerland";
$official->{"Korea"} = "South Korea";
$official->{"Kingdom of Saudi Arabia"} = "Saudi Arabia";
$official->{"Kingodm of Saudi Arabia"} = "Saudi Arabia";
$official->{"Czechia"} = "Czech Republic";
$official->{"México"} = "Mexico";
$official->{"Brasil"} = "Brazil";
$official->{"Türkiye"} = "Turkey";
$official->{"Turkiye"} = "Turkey";
$official->{"Belgique"} = "Belgium";
$official->{"España"} = "Spain";
$official->{"Tunisie"} = "Tunisia";
$official->{"Sverige"} = "Sweden";
$official->{"România"} = "Romania";
$official->{"Italia"} = "Italy";
$official->{"Italie"} = "Italy";
$official->{"Maroc"} = "Morocco";
$official->{"Republic of South Africa"} = "South Africa";
$official->{"Algérie"} = "Algeria";
$official->{"Cameroun"} = "Cameroon";
$official->{"Karnataka"} = "Karnataka";
$official->{"Polska"} = "Poland";
$official->{"Grèce"} = "Greece";
$official->{"Bulgarian"} = "Bulgaria";
$official->{"POLAND"} = "Poland";
$official->{"IRAQ"} = "Iraq";
$official->{"Espagne"} = "Spain";
$official->{"Roumanie"} = "Romania";
$official->{"Canadian"} = "Canada";
$official->{"Argentine"} = "Argentina";
$official->{"U.K"} = "United Kingdom";
$official->{"U.K."} = "United Kingdom";
$official->{"UK"} = "United Kingdom";
$official->{"England"} = "United Kingdom";
$official->{"USA"} = "United States";
$official->{"US"} = "United States";
$official->{"U.S"} = "United States";
$official->{"U.S."} = "United States";
$official->{"U.S.A"} = "United States";
$official->{"United States of America"} = "United States";
$official->{"Czech"} = "Czech Republic";
$official->{"Czechia"} = "Czech Republic";
$official->{"Democratic Republic of the Congo"} = "Republic of the Congo";
$official->{"DR Congo"} = "Republic of the Congo";
$official->{"Congo"} = "Republic of the Congo";
$official->{"Korea South"} = "South Korea";
$official->{"Korea, Republic of"} = "South Korea";
$official->{"Korea, Democratic People's Republic of"} = "South Korea";
$official->{"Kosovo"} = "Bosnia and Herzegovina";
$official->{"Tanzania, United Republic of"} = "Tanzania";
$official->{"Saint Vincent and The Grenadines"} = "Saint Vincent and the Grenadines";
$official->{"Saint Vincent And The Grenadines"} = "Saint Vincent and the Grenadines";
$official->{"Congo Democratic Republic"} = "Republic of the Congo";
$official->{"Congo, the Democratic Republic of the"} = "Republic of the Congo";
$official->{"The Netherlands"} = "Netherlands";
$official->{"Belgien"} = "Belgium";
$official->{"Espana"} = "Spain";
$official->{"Moldova, Republic of"} = "Moldova";
$official->{"Venezuela, Bolivarian Republic of"} = "Venezuela";
$official->{"Bolivia, Plurinational State of"} = "Bolivia";
$official->{"Brunei Darussalam"} = "Brunei";
$official->{"Côte d'Ivoire"} = "Ivory Coast";
$official->{"Palestinian Territory"} = "Palestine";
$official->{"Palestine, State of"} = "Palestine";
$official->{"Syrian Arab Republic"} = "Syria";
$official->{"Timor Leste"} = "Timor-Leste";
$official->{"Viet Nam"} = "Vietnam";
$official->{"Korea North"} = "North Korea";
$official->{"Sicily"} = "Italy";
$official->{"Austrilia"} = "Australia";
$official->{"Macao"} = "Macau";
$official->{"United States Virgin Islands"} = "US Virgin Islands";
$official->{"Virgin Islands (US)"} = "US Virgin Islands";
$official->{"U.S. Virgin Islands"} = "US Virgin Islands";
$official->{"Hong Kong S.A.R."} = "Hong Kong";
$official->{"Hong Kong SAR"} = "Hong Kong";
$official->{"Macao SAR"} = "Macao";
$official->{"Karnataka"} = "India";
$official->{"Azores"} = "Portugal";
$official->{"Canary Islands"} = "Spain";
$official->{"Vatican"} = "Vatican City";
$official->{"Holy See (Vatican City State)"} = "Vatican City";
$official->{"Antigua And Barbuda"} = "Antigua and Barbuda";
$official->{"Cote D'Ivoire (Ivory Coast)"} = "Ivory Coast";
$official->{"Fiji Islands"} = "Fiji";
$official->{"Gambia The"} = "Gambia";
$official->{"Trinidad And Tobago"} = "Trinidad and Tobago";
$official->{"Papua new Guinea"} = "Papua New Guinea";
$official->{"East Timor"} = "Timor-Leste";
$official->{"The Bahamas"} = "Bahamas";
$official->{"Saint Kitts And Nevis"} = "Saint Kitts and Nevis";
$official->{"Réunion"} = "Reunion";
$official->{"Virgin Islands, British"} = "British Virgin Islands";
$official->{"Panamá"} = "Panama";
$official->{"República de Panamá"} = "Panama";
$official->{"Yugoslavia"} = "Serbia";
$official->{"Scotland"} = "United Kingdom";
$official->{"Wales"} = "United Kingdom";
$official->{"England"} = "United Kingdom";
$official->{"the Netherlands"} = "Netherlands";
$official->{"The Netherlands"} = "Netherlands";
$official->{"Republic of Macedonia"} = "North Macedonia";
$official->{"Macedonia"} = "North Macedonia";
$official->{"PR China"} = "China";
$official->{"P.R. China"} = "China";
$official->{"Northern Ireland"} = "United Kingdom";
$official->{"Sénégal"} = "Senegal";
$official->{"Perú"} = "Peru";
$official->{"República Argentina"} = "Argentina";


# https://worldpopulationreview.com/country-rankings/official-names-of-countries
use Text::CSV qw( csv );
my $h = csv(in => "source/official-names-of-countries-2024.csv", headers => "auto");

for(my $i = 0; $i <= $#$h; $i ++) {

	# manual convert to standard names
	if(defined($official->{ $h->[$i]->{country} })) {
		$h->[$i]->{country} = $official->{ $h->[$i]->{country} };
	}

	# long name to short name
	$official->{$h->[$i]->{officialName}} = $h->[$i]->{country};
	$official->{$h->[$i]->{country}} = $h->[$i]->{country};

	# email domain name
	$domain->{lc($h->[$i]->{cca2})} = $h->[$i]->{country};

	$country_code->{$h->[$i]->{cca2}} = $h->[$i]->{country}; ## cca2, e.g. CN for China
	$country_code_cca3->{$h->[$i]->{cca3}} = $h->[$i]->{country}; ## cca3, e.g. CHN for China
}

delete($official->{"Jersey"});
delete($official->{"Georgia"});

# also map country names with all upper case
foreach my $c (keys %$official) {
	$official->{uc($c)} = $official->{$c};
}

## till now, all standard country names are values of %$official

sub normalize_country_name {
	my $name = shift;
	my $official = shift;

	if(defined($official->{$name})) {
		return $official->{$name};
	} else {
		return "";
	}
}

$dict->{"official"} = $official;
$dict->{"domain"} = $country_code;

sub map_to_country_name {
	my $name = shift;
	my $dict = shift;

	my $official = $dict->{"official"};

	normalize_country_name($name, $official);
}


# save standardized country names
open OUT, ">processed/country_meta.tab" or die;
my @fields = ("place","pop2024","growthRate","area","country","cca3","cca2","ccn3","region","subregion","unMember","officialName","landAreaKm","density","densityMi","borders","Rank");
print OUT "standardName"; map {print OUT "\t$_"} @fields; print OUT "\n";
for(my $i = 0; $i <= $#$h; $i ++) {
	my $c = $h->[$i];
	print OUT normalize_country_name($c->{country}, $official);
	map {print OUT "\t$c->{$_}"} @fields; print OUT "\n";
}
close OUT;



# https://github.com/w8r/k-d-tree/blob/master/data/data.tsv
my $city = {};
my $city_alt = {};

open F, "source/world_cities_more.tsv" or die;
while(my $line = <F>) {
	chomp $line;
	my @line = split "\t", $line, -1;

	next if(length($line[1]) < 4);  # nchar of the city name
	next if($line[14] < 10000);

	if(defined($country_code->{$line[8]})) {
		if(defined($city->{$line[1]}->{$country_code->{$line[8]}})) {
			if($city->{$line[1]}->{$country_code->{$line[8]}} < $line[14]) {
				$city->{$line[1]}->{$country_code->{$line[8]}} = $line[14];
			}
		} else {
			$city->{$line[1]}->{$country_code->{$line[8]}} = $line[14];
		}
		if($line[2] ne "" and $line[1] ne $line[2]) {
			$city_alt->{$line[2]}->{$line[1]}->{$country_code->{$line[8]}} = $line[14];
		}

		foreach my $other (split ",", $line[3]) {
			next if(length($other) <= 4);

			if($other =~/[a-z]/) {
				$city_alt->{$other}->{$line[1]}->{$country_code->{$line[8]}} = $line[14];
			}
		}
	} else {
		print STDERR "$line\n";
	}
}
close F;

use Data::Dumper;
# foreach my $x (keys %$city) {
# 	if(scalar %{$city->{$x}} > 1) {
# 		print "$x\n";
# 		print Dumper($city->{$x});
# 	}
# }

sub umlaut_to_letters {
	my $x = shift;
	$x =~s/ä/ae/g;
	$x =~s/ö/oe/g;
	$x =~s/ü/ue/g; 
	$x =~s/ß/ss/g; 

	return $x;
}

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

foreach my $x (keys %$city_alt) {
	my $y = umlaut_to_letters($x);
	if($x ne $y) {
		$city_alt->{$y} = $city_alt->{$x};
	}
}
foreach my $x (keys %$city) {
	my $y = umlaut_to_letters($x);
	if($x ne $y) {
		$city_alt->{$y}->{$x} = $city->{$x};
	}
}


my $city_remove = ["University", "Central", "Mission", "University City"];
foreach my $c (@$city_remove) {
	if(defined($city->{$c})) {
		delete($city->{$c});
	}

	foreach my $foo (keys %$city_alt) {
		if(defined($city_alt->{$foo}->{$c})) {
			delete($city_alt->{$foo}->{$c});
		}
	}
}

$city->{"Pomona Valley"}->{"United States"} = 10000;
$city->{"Hellerup"}->{"Denmark"} = 10000;
$city->{"Meldola"}->{"Italy"} = 10000;
$city->{"Saint Martin d'Hères"}->{"France"} = 38188;
$city->{"Tel Hashomer"}->{"Israel"} = 38188;
$city->{"Tel-Hashomer"}->{"Israel"} = 38188;
$city->{"City of Hope"}->{"United States"} = 38188;
$city->{"Le Kremlin Bicêtre"}->{"France"} = 24000;
$city->{"Le Kremlin-Bicêtre"}->{"France"} = 24000;


$dict->{"city"} = $city;
$dict->{"city_alt"} = $city_alt;



sub normalize_city_name {
	my $name = shift;
	my $country = shift;
	my $city = shift;
	my $city_alt = shift;

	if(defined($city->{$name}) and defined($city->{$name}->{$country})) {
		return $name;
	}

	if(defined($city_alt->{$name})) {
		foreach my $x (keys %{$city_alt->{$name}}) {
			if(defined($city_alt->{$name}->{$x}->{$country})) {
				return $x;
			}
		}
	}

	return "";
}



# https://github.com/rinvex/universities/
use JSON;

my $uni = {};
my $uni_alt = {};

my $state = {};
my $province = {};

foreach my $file (glob("source/universities/resources/*/*.json")) {
	my $fh;
	open $fh, $file;

	my $json = JSON->new;
	my $data = $json->decode(<$fh>);
	close $fh;

	my $c = normalize_country_name($data->{"country"}, $official);
	if($c ne "") {

		next if($data->{"name"} =~/college/i);
		next if($data->{"name"} =~/school/i);

		if(defined($data->{"state"}) and $data->{"state"} eq "Taiwan") {
			$c = "Taiwan";
		} elsif(defined($data->{"state"}) and $data->{"state"} eq "Hong Kong SAR") {
			$c = "Hong Kong";
		} elsif(defined($data->{"state"}) and $data->{"state"} eq "Macao SAR") {
			$c = "Macau";
		} elsif(defined($data->{"state"}) and $data->{"state"} eq "Puerto Rico") {
			$c = "Puerto Rico";
		}

		if(defined($data->{"state"})) {
			if(length($data->{"state"}) > 4) {
				$state->{$data->{"state"}}->{$c} = 1;
			}
		}
		if(defined($data->{"address"}->{"province"})) {
			if(length($data->{"address"}->{"province"}) > 4) {
				$province->{$data->{"address"}->{"province"}}->{$c} = 1;
			}
		}

		my $zz = "";
		if(defined($data->{"address"}->{"city"})) {
			$zz = normalize_city_name($data->{"address"}->{"city"}, $c, $city, $city_alt);
			if($zz ne "") {
				$zz = $data->{"address"}->{"city"};
			} else {
				next;
			}
		} else {
			next;
		}

		$uni->{$data->{"name"}}->{$zz}->{$c} = 1;


		my $nn = $data->{"name"};
		$nn =~s/\(.*\)//g;
		$nn =~s/\s*$//g;
		$uni_alt->{$nn}->{$data->{"name"}} = $uni->{$data->{"name"}};
			
		if(defined($data->{"alt_name"})) {
			my $alt = $data->{"alt_name"};
			if($alt !~/[a-z]/) {
				next;
			}
			$alt =~s/\(.*\)//g;
			$alt =~s/\s*$//g;
			if($alt =~/^(.*) – (.*)$/) {
				$uni_alt->{$1}->{$data->{"name"}} = $uni->{$data->{"name"}};
				$uni_alt->{$2}->{$data->{"name"}} = $uni->{$data->{"name"}};
			} else {
				$uni_alt->{$alt}->{$data->{"name"}} = $uni->{$data->{"name"}};
			}
		}
		
	} else {
		print STDERR "not matched: $data->{'country'}\n";
	}
}


foreach my $x (keys %$uni_alt) {
	my $y = umlaut_to_letters($x);
	if($x ne $y) {
		$uni_alt->{$y} = $uni_alt->{$x};
	}
}

foreach my $x (keys %$uni) {
	if($x =~/^University of (\S+)$/) {
		$uni_alt->{"$1 University"}->{$x} = $uni->{$x};
		$uni_alt->{"The University of $1"}->{$x} = $uni->{$x};
	}
	if($x =~/^The University of (\S+)$/) {
		$uni_alt->{"$1 University"}->{$x} = $uni->{$x};
		$uni_alt->{"University of $1"}->{$x} = $uni->{$x};
	}

	if($x =~/^(\S+) University$/) {
		$uni_alt->{"University of $1"}->{$x} = $uni->{$x};
		$uni_alt->{"The University of $1"}->{$x} = $uni->{$x};
	}
}

foreach my $x (keys %$state) {
	my $y = umlaut_to_letters($x);
	if($x ne $y) {
		$state->{$y} = $state->{$x};
	}
}
foreach my $x (keys %$province) {
	my $y = umlaut_to_letters($x);
	if($x ne $y) {
		$province->{$y} = $province->{$x};
	}
}

$dict->{"uni"} = $uni;
$dict->{"uni_alt"} = $uni_alt;
$dict->{"state"} = $state;
$dict->{"province"} = $province;


sub which_max {
	my $array = shift;

	my $ind = 0;
	my $v = -999999;

	for(my $i = 0; $i < $#$array; $i ++) {
		if($array->[$i] > $v) {
			$v = $array->[$i];
			$ind = $i;
		}
	}
	return $ind;
}

sub find_by_key {
	my $key = shift;
	my $hash = shift;

	my $hash2 = {};

	foreach my $x (keys %$hash) {
		if($x =~/\b$key\b/i) {
			$hash2->{$x} = $hash->{$x};
		}
	}
	return $hash2;
}

# manuall add uni 
$uni->{"The University of Western Ontario"}->{"_city_"}->{"Canada"} = 1;
$uni->{"Loma Linda Medical Center"}->{"_city_"}->{"United States"} = 1;
$uni->{"Mayo Clinic School of Medicine"}->{"_city_"}->{"United States"} = 1;
$uni->{"Mayo Clinic College of Medicine"}->{"_city_"}->{"United States"} = 1;
$uni->{"Mayo Clinic College of Medicine and Science"}->{"_city_"}->{"United States"} = 1;
$uni->{"National Institutes of Health"}->{"Bethesda"}->{"United States"} = 1;
$uni->{"National Cancer Institute"}->{"Bethesda"}->{"United States"} = 1;
$uni->{"Dana-Farber Cancer Institute"}->{"Boston"}->{"United States"} = 1;
$uni->{"Johns Hopkins University School of Medicine"}->{"_city_"}->{"United States"} = 1;
$uni->{"Lawrence Livermore National Laboratory"}->{"_city_"}->{"United States"} = 1;
$uni->{"Lawrence Berkeley National Laboratory"}->{"_city_"}->{"United States"} = 1;
$uni->{"Pitié Salpêtrière Hospital"}->{"_city_"}->{"France"} = 1;
$uni->{"Mayo Clinic"}->{"_city_"}->{"United States"} = 1;
$uni->{"Mayo Clinic Cancer Center"}->{"_city_"}->{"United States"} = 1;
$uni->{"Pázmány Péter Catholic University"}->{"_city_"}->{"Hungary"} = 1;
$uni->{"University of Cape Town"}->{"Cape Town"}->{"South Africa"} = 1;
$uni->{"Chang Gung University"}->{"_city_"}->{"Taiwan"} = 1;
$uni->{"Northwestern University"}->{"_city_"}->{"United States"} = 1;
$uni->{"Universitat Dusseldorf"}->{"Düsseldorf"}->{"Germany"} = 1;
$uni->{"Stanford University School of Medicine"}->{"_city_"}->{"United States"} = 1;
$uni->{"Harvard Medical School"}->{"_city_"}->{"United States"} = 1;
$uni->{"Humanitas University"}->{"Milano"}->{"Italy"} = 1;
$uni->{"National Library of Medicine"}->{"Bethesda"}->{"United States"} = 1;
$uni->{"Westlake University"}->{"Hangzhou"}->{"China"} = 1;
$uni->{"Rutgers University"}->{"_city_"}->{"United States"} = 1;
$uni->{"Beth Israel Deaconess Medical Center"}->{"Boston"}->{"United States"} = 1;
$uni->{"Beth Israel Lahey Health"}->{"Burlington"}->{"United States"} = 1;
$uni->{"Albert Einstein College of Medicine"}->{"Bronx"}->{"United States"} = 1;
$uni->{"Tampere University Hospital"}->{"_city_"}->{"Finland"} = 1;
$uni->{"Turku University Hospital"}->{"Turku"}->{"Finland"} = 1;
$uni->{"China-Japan Friendship Hospital"}->{"Beijing"}->{"China"} = 1;
$uni->{"NC State University"}->{"New York City"}->{"United States"} = 1;
$uni->{"The Fifth Medical Center of PLA General Hospital"}->{"Beijing"}->{"China"} = 1;
$uni->{"University of Genova"}->{"Genoa"}->{"Italy"} = 1;
$uni->{"King Abdullah University of Science and Technology"}->{"Thuwal"}->{"Saudi Arabia"} = 1;
$uni->{"King Abdullah International Medical Research Center"}->{"Thuwal"}->{"Saudi Arabia"} = 1;
$uni->{"Radboud University"}->{"Nijmegen"}->{"Netherlands"} = 1;
$uni->{"Howard Hughes Medical Institute"}->{"Chevy Chase"}->{"United States"} = 1;
$uni->{"Chang Gung University of Science and Technology"}->{"Taoyuan"}->{"Taiwan"} = 1;
$uni->{"National Cheng Kung University Hospital"}->{"Tainan"}->{"Taiwan"} = 1;
$uni->{"Ng Teng Fong General Hospital"}->{"Singapore"}->{"Singapore"} = 1;
$uni->{"Hospital del Mar"}->{"Barcelona"}->{"Spain"} = 1;
$uni->{"Li Ka Shing Knowledge Institute"}->{"Torondo"}->{"Canada"} = 1;
$uni->{"Robert Koch Institute"}->{"Berlin"}->{"Germany"} = 1;
$uni->{"Mount Sinai Hospital"}->{"New York City"}->{"United States"} = 1;
$uni->{"Mount Sinai School of Medicine"}->{"New York City"}->{"United States"} = 1;
$uni->{"Icahn School of Medicine at Mount Sinai"}->{"New York City"}->{"United States"} = 1;
$uni->{"Rush Medical College"}->{"Chicago"}->{"United States"} = 1;
$uni->{"Université Grenoble Alpes"}->{"Saint-Martin-d’Hères"}->{"France"} = 1;
$uni->{"University of North Carolina"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"Campbell University"}->{"Buies Creek"}->{"United States"} = 1;
$uni->{"Institut Camille Jordan"}->{"Villeurbanne"}->{"France"} = 1;
$uni->{"Hospital Universitari Vall d'Hebron"}->{"Barcelona"}->{"Spain"} = 1;
$uni->{"Tel Aviv University"}->{"Tel Aviv"}->{"Israel"} = 1;
$uni->{"Sorbonne Université"}->{"Paris"}->{"France"} = 1;
$uni->{"Azerbaijan Shahid Madani University"}->{"Tabrīz"}->{"Iran"} = 1;
$uni->{"University of Tsukuba"}->{"Tsukuba"}->{"Japan"} = 1;
$uni->{"South Karelia Central Hospital"}->{"Lappeenranta"}->{"Finland"} = 1;
$uni->{"Joint School of National University of Singapore and Tianjin University"}->{"Tianjin"}->{"China"} = 1;
$uni->{"Jersey Shore University Medical Center"}->{"Neptune Township"}->{"United States"} = 1;
$uni->{"Jackson Laboratory"}->{"Bar Harbor"}->{"United States"} = 1;
$uni->{"Friedrich-Schiller University Jena"}->{"Jena"}->{"Germany"} = 1;
$uni->{"Regions Hospitalet Randers"}->{"Aarhus"}->{"Denmark"} = 1;
$uni->{"Aix-Marseille Université"}->{"Marseille"}->{"France"} = 1;
$uni->{"Hotel-Dieu de France University Hospital"}->{"Beirut"}->{"Lebanon"} = 1;
$uni->{"Gangnam Severance Hospital"}->{"Seoul"}->{"South Korea"} = 1;
$uni->{"The Second Xiangya Hospital of Central South University"}->{"Xiangya"}->{"China"} = 1;
$uni->{"Hospital Universitario La Paz"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"Hospital Italiano de Buenos Aires"}->{"_city_"}->{"Argentina"} = 1;
$uni->{"Shaare-Zedek Medical Center"}->{"Jerusalem"}->{"Israel"} = 1;
$uni->{"Mayo Clinic Rochester"}->{"Rochester"}->{"United States"} = 1;
$uni->{"Department of Cardiovascular Medicine Mayo Clinic Rochester MN"}->{"Rochester"}->{"United States"} = 1;
$uni->{"Fraunhofer Institut für Biomedizinische Technik"}->{"Sulzbach"}->{"Germany"} = 1;
$uni->{"Jouf University"}->{"Sakaka"}->{"Saudi Arabia"} = 1;
$uni->{"Hôpital Européen Georges Pompidou"}->{"Paris"}->{"France"} = 1;
$uni->{"Hôpital Lariboisière"}->{"Paris"}->{"France"} = 1;
$uni->{"Hôpital Saint-Antoine"}->{"Paris"}->{"France"} = 1;
$uni->{"Hôpital Tenon"}->{"Paris"}->{"France"} = 1;
$uni->{"Shonan-Kamakura General Hospital"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Hirosaki University School of Medicine"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Hirosaki University"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Nara Institute of Science and Technology"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Tokai University School of Medicine"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Children's Hospital of Alabama"}->{"Birmingham"}->{"United States"} = 1;
$uni->{"Mallinckrodt Institute of Radiology"}->{"St. Louis"}->{"United States"} = 1;
$uni->{"Tri-Service General Hospital"}->{"Taipei"}->{"Taiwan"} = 1;
$uni->{"Instituto Nacional de Cancerología"}->{"Mexico City"}->{"Mexico"} = 1;
$uni->{"Mie University Graduate School of Medicine"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Mie University"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Altnagelvin Hospital"}->{"Derry"}->{"United Kingdom"} = 1;
$uni->{"Yan'an People's Hospital"}->{"Yan'an"}->{"China"} = 1;
$uni->{"University of Alabama at Birmingham School of Medicine"}->{"Birmingham"}->{"United States"} = 1;
$uni->{"Instituto Nacional de Cancerología"}->{"Mexico City"}->{"Mexico"} = 1;
$uni->{"Cambridge University Hospitals"}->{"Cambridge"}->{"United Kingdom"} = 1;
$uni->{"Max Rubner Institute"}->{"Karlsruhe"}->{"Germany"} = 1;
$uni->{"Tsuchiya General Hospital"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Chang Gung University College of Medicine"}->{"Taoyuan"}->{"Taiwan"} = 1;
$uni->{"UC Berkeley"}->{"Berkeley"}->{"United States"} = 1;
$uni->{"University of Tsukuba"}->{"_city_"}->{"Japan"} = 1;
$uni->{"University Hospital Freiburg"}->{"Freiburg"}->{"Germany"} = 1;
$uni->{"University of Sao Paulo"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Faculdade de Medicina da Universidade de São Paulo"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Khalifa University of Science and Technology"}->{"Abu Dhabi"}->{"United Arab Emirates"} = 1;
$uni->{"Wingate University"}->{"Wingate"}->{"United States"} = 1;
$uni->{"Takasaki University of Health and Welfare"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Asahi University"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Chang Gung Memorial Hospital"}->{"_city_"}->{"Taiwan"} = 1;
$uni->{"Hospital Cayetano Heredia"}->{"_city_"}->{"Peru"} = 1;
$uni->{"Tel Aviv Sourasky Medical Center"}->{"Tel Aviv"}->{"Israel"} = 1;
$uni->{"University Hospital Tübingen"}->{"Tübingen"}->{"Germany"} = 1;
$uni->{"Columbia University"}->{"Manhattan"}->{"United States"} = 1;
$uni->{"Columbia University Medical Center"}->{"Manhattan"}->{"United States"} = 1;
$uni->{"Columbia University College of Physicians and Surgeons"}->{"Manhattan"}->{"United States"} = 1;
$uni->{"Columbia University Mailman School of Public Health"}->{"Manhattan"}->{"United States"} = 1;
$uni->{"Institut Pasteur"}->{"Paris"}->{"France"} = 1;
$uni->{"Roswell Park Cancer Institute"}->{"Buffalo"}->{"United States"} = 1;
$uni->{"Memorial Sloan Kettering Cancer Center"}->{"New York City"}->{"United States"} = 1;
$uni->{"Memorial Sloan-Kettering Cancer Center"}->{"New York City"}->{"United States"} = 1;
$uni->{"From Memorial Sloan Kettering Cancer Center"}->{"New York City"}->{"United States"} = 1;
$uni->{"Moffitt Cancer Center"}->{"Tampa"}->{"United States"} = 1;
$uni->{"Dana-Farber/Brigham and Women's Cancer Center"}->{"Boston"}->{"United States"} = 1;
$uni->{"Brigham and Women's Hospital"}->{"Boston"}->{"United States"} = 1;
$uni->{"Baylor College of Medicine"}->{"Houston"}->{"United States"} = 1;
$uni->{"Vanderbilt-Ingram Cancer Center"}->{"Nashville"}->{"United States"} = 1;
$uni->{"Fox Chase Cancer Center"}->{"Philadelphia"}->{"United States"} = 1;
$uni->{"City of Hope Comprehensive Cancer Center"}->{"Duarte"}->{"United States"} = 1;
$uni->{"City of Hope National Medical Center"}->{"Duarte"}->{"United States"} = 1;
$uni->{"UC San Diego Moores Cancer Center"}->{"San Diego"}->{"United States"} = 1;
$uni->{"University of Missouri-St"}->{"St. Louis"}->{"United States"} = 1;
$uni->{"Fred Hutchinson Cancer Research Center"}->{"Seattle"}->{"United States"} = 1;
$uni->{"Fred Hutchinson Cancer Center"}->{"Seattle"}->{"United States"} = 1;
$uni->{"University of Washington/Seattle Cancer Care Alliance"}->{"Seattle"}->{"United States"} = 1;
$uni->{"Fred Hutchinson Cancer Research Center/Seattle Cancer Care Alliance"}->{"Seattle"}->{"United States"} = 1;
$uni->{"University of Rochester Medical Center"}->{"Rochester"}->{"United States"} = 1;
$uni->{"University of Rochester School of Medicine and Dentistry"}->{"Rochester"}->{"United States"} = 1;
$uni->{"Sun Yat-sen University Cancer Center"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"University of Notre Dame"}->{"Notre Dame"}->{"United States"} = 1;
$uni->{"University of Heidelberg"}->{"Heidelberg"}->{"Germany"} = 1;
$uni->{"University Hospital Heidelberg"}->{"Heidelberg"}->{"Germany"} = 1;
$uni->{"University of the Western Cape"}->{"Bellville"}->{"South Africa"} = 1;
$uni->{"Université Libre de Bruxelles"}->{"Bruxelles"}->{"Belgium"} = 1;
$uni->{"VU University Medical Center"}->{"Amsterdam"}->{"Netherlands"} = 1;
$uni->{"Boehringer Ingelheim Pharma GmbH & Co"}->{"Biberach an der Riß"}->{"Germany"} = 1;
$uni->{"Institut National de la Santé et de la Recherche Médicale"}->{"Paris"}->{"France"} = 1;
$uni->{"Centre de Recherche des Cordeliers"}->{"Paris"}->{"France"} = 1;
$uni->{"Saint Louis University School of Medicine"}->{"St. Louis"}->{"United States"} = 1;
$uni->{"Wayne State University School of Medicine"}->{"Detroit"}->{"United States"} = 1;
$uni->{"Odense University Hospital"}->{"Odense"}->{"Denmark"} = 1;
$uni->{"University of Stellenbosch"}->{"Stellenbosch"}->{"South Africa"} = 1;
$uni->{"Fred & Pamela Buffett Cancer Center"}->{"Omaha"}->{"United States"} = 1;
$uni->{"University of Torino"}->{"Turin"}->{"Italy"} = 1;
$uni->{"Università degli Studi di Torino"}->{"Turin"}->{"Italy"} = 1;
$uni->{"University of South Carolina"}->{"Columbia"}->{"United States"} = 1;
$uni->{"Diakonhjemmet Hospital"}->{"Oslo"}->{"Norway"} = 1;
$uni->{"Second Military Medical University"}->{"Shanghai"}->{"China"} = 1;
$uni->{"Centre National de la Recherche Scientifique"}->{"Paris"}->{"France"} = 1;
$uni->{"Sunnybrook Health Sciences Centre"}->{"Torondo"}->{"Canada"} = 1;
$uni->{"University of Ulm"}->{"Ulm"}->{"Germany"} = 1;
$uni->{"University Hospital Zurich"}->{"Zürich"}->{"Switzerland"} = 1;
$uni->{"University of Padova"}->{"Padua"}->{"Italy"} = 1;
$uni->{"Keele University"}->{"Keele"}->{"United Kingdom"} = 1;
$uni->{"Dartmouth College"}->{"Hanover"}->{"United States"} = 1;
$uni->{"Geisel School of Medicine at Dartmouth"}->{"Hanover"}->{"United States"} = 1;
$uni->{"Northeastern University"}->{"Boston"}->{"United States"} = 1;
$uni->{"Wageningen University"}->{"Wageningen"}->{"Netherlands"} = 1;
$uni->{"Université Catholique de Louvain"}->{"Ottignies-Louvain-la-Neuve"}->{"Belgium"} = 1;
$uni->{"University of California-Los Angeles"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"UCLA"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"University of California-Los Angeles"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"Universidad Autónoma Metropolitana-Iztapalapa"}->{"Alcaldía Iztapalapa"}->{"Mexico"} = 1;
$uni->{"Tufts Medical Center"}->{"Boston"}->{"United States"} = 1;
$uni->{"Sun Yat-sen University"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"Sun Yat-sen University Cancer Center"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"Polish Academy of Sciences"}->{"Warsaw"}->{"Poland"} = 1;
$uni->{"University Medical Center Groningen"}->{"Groningen"}->{"Netherlands"} = 1;
$uni->{"University of North Carolina at Chapel Hill School of Dentistry"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"University of North Carolina-Chapel Hill"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"University of North Carolina School of Medicine"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"Fourth Military Medical University"}->{"Xi'an"}->{"China"} = 1;
$uni->{"Thames Valley University"}->{"London"}->{"United Kingdom"} = 1;
$uni->{"Marmara University"}->{"İstanbul"}->{"Turkey"} = 1;
$uni->{"Torrey Pines Institute for Molecular Studies"}->{"_city_"}->{"United States"} = 1;
$uni->{"Petersburg State University"}->{"St Petersburg"}->{"Russia"} = 1;
$uni->{"Hofstra North Shore-LIJ School of Medicine"}->{"New York City"}->{"United States"} = 1;
$uni->{"University of Regensburg"}->{"Regensburg"}->{"Germany"} = 1;
$uni->{"University Hospital Regensburg"}->{"Regensburg"}->{"Germany"} = 1;
$uni->{"Hong Kong Special Administrative Region"}->{"Hong Kong"}->{"Hong Kong"} = 1;
$uni->{"University Hospital Ulm"}->{"Ulm"}->{"Germany"} = 1;
$uni->{"University College Cork"}->{"Cork"}->{"Ireland"} = 1;
$uni->{"University of São Paulo Medical School"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Institut Jules Bordet"}->{"Bruxelles"}->{"Belgium"} = 1;
$uni->{"Cedars-Sinai Medical Center"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"King Faisal University"}->{"AlAhsa"}->{"Saudi Arabia"} = 1;
$uni->{"David Geffen School of Medicine"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"Children's Hospital Los Angeles"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"Kurume University School of Medicine"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Sheba Medical Center"}->{"Tel Aviv"}->{"Israel"} = 1;
$uni->{"Taif University"}->{"Ta'if"}->{"Saudi Arabia"} = 1;
$uni->{"Perelman School of Medicine"}->{"Philadelphia"}->{"United States"} = 1;
$uni->{"Wake Forest School of Medicine"}->{"Winston-Salem"}->{"United States"} = 1;
$uni->{"IRCCS San Raffaele Scientific Institute"}->{"_city_"}->{"Italy"} = 1;
$uni->{"San Raffaele Scientific Institute"}->{"_city_"}->{"Italy"} = 1;
$uni->{"Murdoch Children's Research Institute"}->{"Melbourne"}->{"Australia"} = 1;
$uni->{"Cold Spring Harbo"}->{"Long Island"}->{"United States"} = 1;
$uni->{"Cold Spring Harbor Laboratory"}->{"Long Island"}->{"United States"} = 1;
$uni->{"UCL Institute of Neurology"}->{"London"}->{"United Kingdom"} = 1;
$uni->{"UCL Queen Square Institute of Neurology"}->{"London"}->{"United Kingdom"} = 1;
$uni->{"Peking Union Medical College Hospital"}->{"Beijing"}->{"China"} = 1;
$uni->{"Albany Medical College"}->{"New York City"}->{"United States"} = 1;
$uni->{"University of South Dakota Sanford School of Medicine"}->{"Sioux Falls"}->{"United States"} = 1;
$uni->{"Mailman School of Public Health"}->{"New York City"}->{"United States"} = 1;
$uni->{"NYU Grossman School of Medicine"}->{"New York City"}->{"United States"} = 1;
$uni->{"NYU Langone Health"}->{"New York City"}->{"United States"} = 1;
$uni->{"Nara Medical University"}->{"_city_"}->{"Japan"} = 1;
$uni->{"Princess Margaret Cancer Centre"}->{"Torondo"}->{"Canada"} = 1;
$uni->{"Université Paris-Saclay"}->{"Paris"}->{"France"} = 1;
$uni->{"Hospital General Universitario Gregorio Marañón"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"Westchester Medical Center"}->{"Valhalla"}->{"United States"} = 1;
$uni->{"Cohen Children's Medical Center"}->{"New York City"}->{"United States"} = 1;
$uni->{"Qassim University"}->{"Buraydah"}->{"Saudi Arabia"} = 1;
$uni->{"Istituto Giannina Gaslini"}->{"Genua"}->{"Italy"} = 1;
$uni->{"Bispebjerg and Frederiksberg Hospital"}->{"Frederiksberg"}->{"Denmark"} = 1;
$uni->{"Westmead Hospital"}->{"Sydney"}->{"Australia"} = 1;
$uni->{"Stellenbosch University"}->{"Stellenbosch"}->{"South Africa"} = 1;
$uni->{"Donald and Barbara Zucker School of Medicine at Hofstra/Northwell"}->{"Hempstead"}->{"United States"} = 1;
$uni->{"Case Western Reserve University School of Medicine"}->{"Cleveland"}->{"United States"} = 1;
$uni->{"Wayne State University School of Medicine"}->{"Detroit"}->{"United States"} = 1;
$uni->{"University Hospital Basel"}->{"Basel"}->{"Switzerland"} = 1;
$uni->{"University Medical Center Göttingen"}->{"Göttingen"}->{"Germany"} = 1;
$uni->{"Rutgers New Jersey Medical School"}->{"Rutgers"}->{"United States"} = 1;
$uni->{"UCSF"}->{"San Francisco"}->{"United States"} = 1;
$uni->{"University of California-San Francisco"}->{"San Francisco"}->{"United States"} = 1;
$uni->{"UC San Diego"}->{"San Diego"}->{"United States"} = 1;
$uni->{"Heidelberg University Hospital"}->{"Heidelberg"}->{"Germany"} = 1;
$uni->{"Loma Linda University School of Medicine"}->{"Loma Linda"}->{"United States"} = 1;
$uni->{"Feinstein Institute for Medical Research"}->{"Manhasset"}->{"United States"} = 1;
$uni->{"Kennedy Krieger Institute"}->{"Baltimore"}->{"United States"} = 1;
$uni->{"Instituto de Salud Carlos III"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"Lineberger Comprehensive Cancer Center"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"University of North Carolina Chapel Hill"}->{"Chapel Hill"}->{"United States"} = 1;
$uni->{"Sapienza University"}->{"Roma"}->{"Italy"} = 1;
$uni->{"UC Davis"}->{"Davis"}->{"United States"} = 1;
$uni->{"University of California-Davis"}->{"Davis"}->{"United States"} = 1;
$uni->{"CHU Montpellier"}->{"Montpellier"}->{"France"} = 1;
$uni->{"Sunnybrook Research Institute"}->{"Torondo"}->{"Canada"} = 1;
$uni->{"Argonne National Laboratory"}->{"Lemont"}->{"United States"} = 1;
$uni->{"Royal Prince Alfred Hospital"}->{"Sydney"}->{"Australia"} = 1;
$uni->{"Ulm University"}->{"Ulm"}->{"Germany"} = 1;
$uni->{"University Hospital Erlangen"}->{"Erlangen"}->{"Germany"} = 1;
$uni->{"Universitätsklinikum Erlangen"}->{"Erlangen"}->{"Germany"} = 1;
$uni->{"CHU de Bordeaux"}->{"Bordeaux"}->{"France"} = 1;
$uni->{"Université de Bordeaux"}->{"Bordeaux"}->{"France"} = 1;
$uni->{"University of Bordeaux"}->{"Bordeaux"}->{"France"} = 1;
$uni->{"Bordeaux University Hospital"}->{"Bordeaux"}->{"France"} = 1;
$uni->{"Instituto Mexicano del Seguro Social"}->{"Coahuila"}->{"Mexico"} = 1;
$uni->{"Third Military Medical University"}->{"Chongqing"}->{"China"} = 1;
$uni->{"Haukeland University Hospital"}->{"Bergen"}->{"Norway"} = 1;
$uni->{"Penn State University"}->{"State College"}->{"United States"} = 1;
$uni->{"Penn State College of Medicine"}->{"State College"}->{"United States"} = 1;
$uni->{"Gifu Pharmaceutical University"}->{"Gifu"}->{"Japan"} = 1;
$uni->{"Feinstein Institutes for Medical Research"}->{"Manhasset"}->{"United States"} = 1;
$uni->{"The Feinstein Institute for Medical Research"}->{"Manhasset"}->{"United States"} = 1;
$uni->{"Uniwersytet Jagielloński Collegium Medicum"}->{"Krakau"}->{"Poland"} = 1;
$uni->{"Rhode Island Hospital"}->{"Rhode Island"}->{"United States"} = 1;
$uni->{"Walter Reed National Military Medical Center"}->{"Bethesda"}->{"United States"} = 1;
$uni->{"Duke-NUS Medical School"}->{"Singapore"}->{"Singapore"} = 1;
$uni->{"Centro Hospitalar e Universitário de Coimbra"}->{"Coimbra"}->{"Portugal"} = 1;
$uni->{"Aix-Marseille University"}->{"Marseille"}->{"France"} = 1;
$uni->{"Brookhaven National Laboratory"}->{"Upton"}->{"United States"} = 1;
$uni->{"NYU School of Medicine"}->{"New York City"}->{"United States"} = 1;
$uni->{"Hôpital Ambroise Paré"}->{"Boulogne-Billancourt"}->{"France"} = 1;
$uni->{"University Federico II"}->{"Neapel"}->{"Italy"} = 1;
$uni->{"Hospital Universitario 12 de Octubre"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"Hospital de la Santa Creu i Sant Pau"}->{"Barcelona"}->{"Spain"} = 1;
$uni->{"Rouen University Hospital"}->{"Rouen"}->{"France"} = 1;
$uni->{"North Shore University Hospital"}->{"Manhasset"}->{"United States"} = 1;
$uni->{"Jena University Hospital"}->{"Jena"}->{"Germany"} = 1;
$uni->{"Sun Yat-sen Memorial Hospital"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"Hôpital Pitié-Salpêtrière"}->{"Paris"}->{"France"} = 1;
$uni->{"University Hospital Cologne"}->{"Cologne"}->{"Germany"} = 1;
$uni->{"University of Louisville School of Medicine"}->{"Louisville"}->{"United States"} = 1;
$uni->{"Sahlgrenska Academy at University of Gothenburg"}->{"Göteborg"}->{"Sweden"} = 1;
$uni->{"Sahlgrenska University Hospital"}->{"Göteborg"}->{"Sweden"} = 1;
$uni->{"The Jikei University School of Medicine"}->{"Tokyo"}->{"Japan"} = 1;
$uni->{"Hoshi University"}->{"Tokyo"}->{"Japan"} = 1;
$uni->{"Montefiore Medical Center"}->{"New York City"}->{"United States"} = 1;
$uni->{"UT Southwestern Medical Center"}->{"Dallas"}->{"United States"} = 1;
$uni->{"Zucker Hillside Hospital"}->{"New York City"}->{"United States"} = 1;
$uni->{"The State University of New Jersey"}->{"New Brunswick"}->{"United States"} = 1;
$uni->{"Normandie Université"}->{"Normandie"}->{"France"} = 1;
$uni->{"Loma Linda University Medical Center"}->{"Loma Linda"}->{"United States"} = 1;
$uni->{"Jagiellonian University Medical College"}->{"Krakau"}->{"Poland"} = 1;
$uni->{"Woods Hole Oceanographic Institution"}->{"Woods Hole"}->{"United States"} = 1;
$uni->{"Lerner Research Institute"}->{"Cleveland"}->{"United States"} = 1;
$uni->{"University Hospital Schleswig-Holstein"}->{"Kiel"}->{"Germany"} = 1;
$uni->{"Hofstra Northwell School of Medicine"}->{"New York City"}->{"United States"} = 1;
$uni->{"Tisch Cancer Institute"}->{"New York City"}->{"United States"} = 1;
$uni->{"Josai University"}->{"Saitama"}->{"Japan"} = 1;
$uni->{"University Hospital Würzburg"}->{"Würzburg"}->{"Germany"} = 1;
$uni->{"Medical University of Lodz"}->{"Lodz"}->{"Poland"} = 1;
$uni->{"Peter MacCallum Cancer Centre"}->{"Melbourne"}->{"Australia"} = 1;
$uni->{"Nottingham University Hospitals NHS Trust"}->{"Nottingham"}->{"United Kingdom"} = 1;
$uni->{"John Radcliffe Hospital"}->{"Oxford"}->{"United Kingdom"} = 1;
$uni->{"Third Affiliated Hospital of Sun Yat-sen University"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"Pitié-Salpêtrière Hospital"}->{"Paris"}->{"France"} = 1;
$uni->{"Fondazione Policlinico Universitario A"}->{"Roma"}->{"Italy"} = 1;
$uni->{"CHU de Besançon"}->{"Besançon Cedex"}->{"France"} = 1;
$uni->{"CHU Clermont-Ferrand"}->{"Clermont-Ferrand"}->{"France"} = 1;
$uni->{"CHU de Caen"}->{"Caen"}->{"France"} = 1;
$uni->{"CHU Grenoble Alpes"}->{"La Tronche"}->{"France"} = 1;
$uni->{"Caen University Hospital"}->{"Caen"}->{"France"} = 1;
$uni->{"ETH Zurich"}->{"Zurich"}->{"Switzerland"} = 1;
$uni->{"Cambridge University Hospitals NHS Foundation Trust"}->{"Cambridge"}->{"United Kingdom"} = 1;
$uni->{"Yong Loo Lin School of Medicine"}->{"Singapore"}->{"Singapore"} = 1;
$uni->{"Umm Al-Qura University"}->{"Makkah"}->{"Saudi Arabia"} = 1;
$uni->{"Al-Baha University"}->{"Al Bahah"}->{"Saudi Arabia"} = 1;
$uni->{"Prince of Wales Hospital"}->{"Randwick"}->{"Australia"} = 1;
$uni->{"São Paulo State University"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Polish Academy of Sciences"}->{"Warsaw"}->{"Poland"} = 1;
$uni->{"Mukogawa Women's University"}->{"Nishinomiya"}->{"Japan"} = 1;
$uni->{"Hospital Universitario Marqués de Valdecilla"}->{"Santander"}->{"Spain"} = 1;
$uni->{"Max Planck Institute"}->{"Berlin"}->{"Germany"} = 1;
$uni->{"Royal North Shore Hospital"}->{"Sydney"}->{"Germany"} = 1;
$uni->{"Changhai Hospital"}->{"Shanghai"}->{"China"} = 1;
$uni->{"Changhua Christian Hospital"}->{"Changhua"}->{"Taiwan"} = 1;
$uni->{"San Francisco Veterans Affairs Medical Center"}->{"San Francisco"}->{"United States"} = 1;
$uni->{"Institut d'Investigacions Biomèdiques August Pi i Sunyer"}->{"Barcelona"}->{"Spain"} = 1;
$uni->{"Hospital Universitario Ramón y Cajal"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"UT Health San Antonio"}->{"San Antonio"}->{"United States"} = 1;
$uni->{"Oxford University Hospitals NHS Foundation Trust"}->{"Oxford"}->{"United Kingdom"} = 1;
$uni->{"Imperial College Healthcare NHS Trust"}->{"London"}->{"United Kingdom"} = 1;
$uni->{"Universidade de Sao Paulo"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Goethe University Frankfurt"}->{"Frankfurt"}->{"Germany"} = 1;
$uni->{"Goethe University"}->{"Frankfurt"}->{"Germany"} = 1;
$uni->{"The First Affiliated Hospital of Xi'an Jiaotong University"}->{"Xi'an"}->{"China"} = 1;
$uni->{"Antwerp University Hospital"}->{"Antwerp"}->{"Belgium"} = 1;
$uni->{"Children's Hospital of Los Angeles"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"Veterans Affairs Greater Los Angeles Healthcare System"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"The Third Affiliated Hospital of Sun Yat-sen University"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"The First Affiliated Hospital of Sun Yat-sen University"}->{"Guangzhou"}->{"China"} = 1;
$uni->{"University Hospital of Cologne"}->{"Cologne"}->{"Germany"} = 1;
$uni->{"Bern University Hospital"}->{"Bern"}->{"Germany"} = 1;
$uni->{"Universitätsklinikum Würzburg"}->{"Würzburg"}->{"Germany"} = 1;
$uni->{"IRCCS Ospedale San Raffaele"}->{"_city_"}->{"Italy"} = 1;
$uni->{"MD Anderson Cancer Center"}->{"Houston"}->{"United States"} = 1;
$uni->{"Universitätsklinikum Jena"}->{"Jena"}->{"Germany"} = 1;
$uni->{"University Hospital Frankfurt"}->{"Frankfurt"}->{"Germany"} = 1;
$uni->{"NYU Langone Medical Center"}->{"New York City"}->{"United State"} = 1;
$uni->{"Oulu University Hospital"}->{"Oulu"}->{"Finland"} = 1;
$uni->{"Tongji Hospital"}->{"Wuhan"}->{"China"} = 1;
$uni->{"Oxford University Hospitals"}->{"Oxford"}->{"United Kingdom"} = 1;
$uni->{"Peking Union Medical College"}->{"Beijing"}->{"China"} = 1;
$uni->{"Rutgers Robert Wood Johnson Medical School"}->{"New Brunswick"}->{"United States"} = 1;
$uni->{"Université de Lille"}->{"Lille"}->{"France"} = 1;
$uni->{"University Hospital Carl Gustav Carus"}->{"Dresden"}->{"Germany"} = 1;
$uni->{"Charité University Hospital"}->{"Berlin"}->{"Germany"} = 1;
$uni->{"Pacific Northwest National Laboratory"}->{"Richland"}->{"United States"} = 1;
$uni->{"PSL Research University"}->{"Paris"}->{"France"} = 1;
$uni->{"Staten Island University Hospital"}->{"New York City"}->{"United States"} = 1;
$uni->{"Tan Tock Seng Hospital"}->{"Singapore"}->{"Singapore"} = 1;
$uni->{"Liaquat University of Medical and Health Sciences"}->{"Sindh"}->{"Pakistan"} = 1;
$uni->{"Fondazione IRCCS Ca' Granda Ospedale Maggiore Policlinico"}->{"Milano"}->{"Italy"} = 1;
$uni->{"Majmaah University"}->{"Al Majma'ah"}->{"Saudi Arabia"} = 1;
$uni->{"Stavanger University Hospital"}->{"Stavanger"}->{"Norway"} = 1;
$uni->{"Yale-New Haven Hospital"}->{"New Haven"}->{"United States"} = 1;
$uni->{"Northern Border University"}->{"Arar"}->{"Saudi Arabia"} = 1;
$uni->{"Marianna University School of Medicine"}->{"Kanagawa"}->{"Japan"} = 1;
$uni->{"Roswell Park Comprehensive Cancer Center"}->{"Buffalo"}->{"United States"} = 1;
$uni->{"Xiangya Hospital"}->{"Changsha"}->{"China"} = 1;
$uni->{"Katedra Psychiatrii"}->{"_city_"}->{"Poland"} = 1;
$uni->{"Sorbonne Universités"}->{"Paris"}->{"France"} = 1;
$uni->{"Centre de Recherche des Cordeliers"}->{"Paris"}->{"France"} = 1;
$uni->{"Addenbrooke's Hospital"}->{"Cambridge"}->{"United Kingdom"} = 1;
$uni->{"Tokai University"}->{"Tokyo"}->{"Japan"} = 1;
$uni->{"Hospital Universitario Virgen del Rocío"}->{"Sevilla"}->{"Spain"} = 1;
$uni->{"SUNY Upstate Medical University"}->{"Syracuse"}->{"United States"} = 1;
$uni->{"Madigan Army Medical Center"}->{"Washington"}->{"United States"} = 1;
$uni->{"Jeju National University Hospital"}->{"Jeju-do"}->{"South Korea"} = 1;
$uni->{"Lankenau Medical Center"}->{"Wynnewood"}->{"United States"} = 1;
$uni->{"Hospital Israelita Albert Einstein"}->{"São Paulo"}->{"Brazil"} = 1;
$uni->{"Carver College of Medicine"}->{"Iowa City"}->{"United States"} = 1;
$uni->{"Lille University Hospital"}->{"Lille"}->{"France"} = 1;
$uni->{"Vagelos College of Physicians and Surgeons"}->{"New York City"}->{"United States"} = 1;
$uni->{"King Abdulaziz Medical City"}->{"Toronto"}->{"Canada"} = 1;
$uni->{"Consejo Superior de Investigaciones Científicas"}->{"Madrid"}->{"Spain"} = 1;
$uni->{"Bambino Gesù Children's Hospital"}->{"Roma"}->{"Italy"} = 1;
$uni->{"Uniwersytet Medyczny im"}->{"_city_"}->{"Poland"} = 1;
$uni->{"University Hospital Muenster"}->{"Münster"}->{"Germany"} = 1;
$uni->{"Akron Children's Hospital"}->{"Akron"}->{"United States"} = 1;
$uni->{"Kuopio University Hospital"}->{"Kuopio"}->{"Finland"} = 1;
$uni->{"Ross University School of Veterinary Medicine"}->{"St. Kitts und Nevis"}->{"United States"} = 1;
$uni->{"Krembil Research Institute"}->{"Torondo"}->{"Canada"} = 1;
$uni->{"Edith Cowan University"}->{"Perth"}->{"Australia"} = 1;
$uni->{"Khon Kaen University"}->{"Khon Kaen"}->{"Thailand"} = 1;
$uni->{"Chung Shan Medical University Hospital"}->{"Taichung City"}->{"Taiwan"} = 1;
$uni->{"CHU de Dijon"}->{"_city_"}->{"France"} = 1;
$uni->{"The Norwegian Radium Hospital"}->{"Oslo"}->{"Norway"} = 1;
$uni->{"Showa Pharmaceutical University"}->{"Machida"}->{"Japan"} = 1;
$uni->{"Jagiellonian University"}->{"Kraków"}->{"Poland"} = 1;
$uni->{"Groupe Hospitalier Pitié-Salpêtrière"}->{"Paris"}->{"France"} = 1;
$uni->{"Rennes University Hospital"}->{"Rennes"}->{"France"} = 1;
$uni->{"Dartmouth-Hitchcock Medical Center"}->{"Lebanon"}->{"United States"} = 1;
$uni->{"UCL Great Ormond Street Institute of Child Health"}->{"London"}->{"United States"} = 1;
$uni->{"Donders Institute for Brain"}->{"Nijmegen"}->{"Netherlands"} = 1;
$uni->{"Centre Léon Bérard"}->{"Lyon"}->{"France"} = 1;
$uni->{"University Hospital Southampton NHS Foundation Trust"}->{"Southampton"}->{"United Kingdom"} = 1;
$uni->{"Simmons Comprehensive Cancer Center"}->{"Dallas"}->{"United States"} = 1;
$uni->{"Wake Forest University School of Medicine"}->{"Winston-Salem"}->{"United States"} = 1;
$uni->{"Great Ormond Street Hospital"}->{"London"}->{"United Kingdom"} = 1;
$uni->{"Children's Mercy Hospital"}->{"Kansas City"}->{"United States"} = 1;
$uni->{"University of the Ryukyus"}->{"Okinawa"}->{"Japan"} = 1;
$uni->{"Max Planck Institute for Psycholinguistics"}->{"Nijmegen"}->{"Netherlands"} = 1;
$uni->{"Binghamton University"}->{"Binghamton"}->{"United States"} = 1;
$uni->{"Lady Davis Institute for Medical Research"}->{"Montréal"}->{"United States"} = 1;
$uni->{"Hospital Universitario de Canarias"}->{"Santa Cruz de Tenerife"}->{"Spain"} = 1;
$uni->{"Geneva University Hospitals"}->{"Genf"}->{"Switzerland"} = 1;
$uni->{"University at Albany"}->{"Albany"}->{"United States"} = 1;
$uni->{"NewYork-Presbyterian Hospital"}->{"New York City"}->{"United States"} = 1;
$uni->{"Hôpital de la Timone"}->{"Marseille"}->{"France"} = 1;
$uni->{"Norwegian Institute of Public Health"}->{"Oslo"}->{"United Kingdom"} = 1;
$uni->{"Lankenau Institute for Medical Research"}->{"Wynnewood"}->{"United States"} = 1;
$uni->{"Keck School of Medicine"}->{"Los Angeles"}->{"United States"} = 1;
$uni->{"University of Stellenbosch"}->{"Stellenbosch"}->{"South Africa"} = 1;
$uni->{"South African Medical Research Council Centre for Tuberculosis Research"}->{"Tygerberg"}->{"South Africa"} = 1;
$uni->{"Barnes-Jewish Hospital"}->{"St. Louis"}->{"United States"} = 1;
$uni->{"Stony Brook University Hospital"}->{"Stony Brook"}->{"United States"} = 1;
$uni->{"King Saud Bin Abdulaziz University for Health Sciences"}->{"Riyadh"}->{"Saudi Arabia"} = 1;
$uni->{"Clinica de La Costa Ltda"}->{"Atlántico"}->{"Columbia"} = 1;
$uni->{"CHU de Montpellier"}->{"Montpellier"}->{"France"} = 1;
$uni->{"University of Missouri-Kansas City School of Medicine"}->{"Kansas City"}->{"United States"} = 1;
$uni->{"Prince Sattam Bin Abdulaziz University"}->{"Al-Kharj"}->{"United States"} = 1;
$uni->{"Hassan II University Hospital"}->{"_city_"}->{"Morocco"} = 1;
$uni->{"Cliniques Universitaires Saint-Luc"}->{"Bruxelles"}->{"Belgium"} = 1;
$uni->{"University of Rochester School of Medicine"}->{"Rochester"}->{"United States"} = 1;
$uni->{"Unnan City Hospital"}->{"Shimane"}->{"Japan"} = 1;
$uni->{"UC San Francisco"}->{"San Francisco"}->{"United States"} = 1;
$uni->{"Biomedical Research Institute Sant Pau"}->{"Barcelona"}->{"Spain"} = 1;
$uni->{"University of Minho"}->{"Braga"}->{"Portugal"} = 1;
$uni->{"Istituto Italiano di Tecnologia"}->{"Genova"}->{"Italy"} = 1;
$uni->{"Pusan National University Hospital"}->{"Busan"}->{"South Korea"} = 1;
$uni->{"Cirugía General y del Aparato Digestivo"}->{"_city_"}->{"Spain"} = 1;
$uni->{"Olive View-UCLA Medical Center"}->{"Sylmar"}->{"United States"} = 1;
$uni->{"National Yang Ming Chiao Tung University"}->{"Hsinchu City"}->{"Taiwan"} = 1;
$uni->{"Wageningen University"}->{"Wageningen"}->{"Netherlands"} = 1;
$uni->{"University of Hail"}->{"Hail"}->{"Saudi Arabia"} = 1;
$uni->{"Meiji Pharmaceutical University"}->{"Tokyo"}->{"Japan"} = 1;
$uni->{"Centro Hospitalar Universitário de São João"}->{"Porto"}->{"Portugal"} = 1;
$uni->{"Akershus University Hospital"}->{"Nordbyhagen"}->{"Norway"} = 1;
$uni->{"Murdoch University"}->{"Perth"}->{"Australia"} = 1;
$uni->{"Dalla Lana School of Public Health"}->{"Toronto"}->{"Canada"} = 1;
$uni->{"St Jude Children's Research Hospital"}->{"Memphis"}->{"United States"} = 1;
$uni->{"Sidi Mohamed Ben Abdellah University"}->{"Fès"}->{"Morocco"} = 1;
$uni->{"Beni-Suef University"}->{"Beni-Suef"}->{"Egypt"} = 1;



delete($uni_alt->{"University of Technology"});
delete($uni->{"University of Medical Sciences"});
delete($uni_alt->{"University of Medical Sciences"});
delete($uni_alt->{"University of Applied Sciences"});
delete($uni_alt->{"Northwestern University"}->{"Northwestern University (Philippines)"});
delete($uni_alt->{"Western University"}->{"Western University (Azerbaijan)"});
delete($uni_alt->{"Western University"}->{"Western University (Cambodia)"});
delete($uni_alt->{"Western University"}->{"Western University (Thailand)"});


## manually US states to countries
my $usa_states = {};
$usa_states->{"Alaska"} = "United States";
$usa_states->{"Alabama"} = "United States";
$usa_states->{"Arkansas"} = "United States";
$usa_states->{"American Samoa"} = "United States";
$usa_states->{"Arizona"} = "United States";
$usa_states->{"California"} = "United States";
$usa_states->{"Colorado"} = "United States";
$usa_states->{"Connecticut"} = "United States";
$usa_states->{"District of Columbia"} = "United States";
$usa_states->{"Delaware"} = "United States";
$usa_states->{"Florida"} = "United States";
# $usa_states->{"Georgia"} = "United States";
$usa_states->{"Guam"} = "United States";
$usa_states->{"Hawaii"} = "United States";
$usa_states->{"Iowa"} = "United States";
$usa_states->{"Idaho"} = "United States";
$usa_states->{"Illinois"} = "United States";
$usa_states->{"Indiana"} = "United States";
$usa_states->{"Kansas"} = "United States";
$usa_states->{"Kentucky"} = "United States";
$usa_states->{"Louisiana"} = "United States";
$usa_states->{"Massachusetts"} = "United States";
$usa_states->{"Maryland"} = "United States";
$usa_states->{"Maine"} = "United States";
$usa_states->{"Michigan"} = "United States";
$usa_states->{"Minnesota"} = "United States";
$usa_states->{"Missouri"} = "United States";
$usa_states->{"Mississippi"} = "United States";
$usa_states->{"Montana"} = "United States";
$usa_states->{"North Carolina"} = "United States";
$usa_states->{"North Dakota"} = "United States";
$usa_states->{"Nebraska"} = "United States";
$usa_states->{"New Hampshire"} = "United States";
$usa_states->{"New Jersey"} = "United States";
$usa_states->{"New Mexico"} = "United States";
$usa_states->{"Nevada"} = "United States";
$usa_states->{"New York"} = "United States";
$usa_states->{"Ohio"} = "United States";
$usa_states->{"Oklahoma"} = "United States";
$usa_states->{"Oregon"} = "United States";
$usa_states->{"Pennsylvania"} = "United States";
$usa_states->{"Puerto Rico"} = "United States";
$usa_states->{"Rhode Island"} = "United States";
$usa_states->{"South Carolina"} = "United States";
$usa_states->{"South Dakota"} = "United States";
$usa_states->{"Tennessee"} = "United States";
$usa_states->{"Texas"} = "United States";
$usa_states->{"Utah"} = "United States";
$usa_states->{"Virginia"} = "United States";
$usa_states->{"Virgin Islands"} = "United States";
$usa_states->{"Vermont"} = "United States";
$usa_states->{"Washington"} = "United States";
$usa_states->{"Wisconsin"} = "United States";
$usa_states->{"West Virginia"} = "United States";
$usa_states->{"Wyoming"} = "United States";

foreach my $c (keys %$usa_states) {
	$usa_states->{uc($c)} = $usa_states->{$c};
}

$dict->{"usa_states"} = $usa_states;

sub map_to_usa_states {
	my $name = shift;
	my $dict = shift;

	if(defined($dict->{"usa_states"}->{$name})) {
		return $dict->{"usa_states"}->{$name};
	} else {
		return "";
	}
}

sub map_to_province {
	my $name = shift;
	my $dict = shift;

	if(defined($dict->{"province"}->{$name})) {
		return $dict->{"province"}->{$name};
	} elsif(defined($dict->{"state"}->{$name})) {
		return $dict->{"state"}->{$name};
	} else {
		return undef;
	}
}



sub first_value {
	my $hash = shift;

	my $first_key = (keys %$hash)[0];

	$hash->{$first_key};
}

sub first_key {
	my $hash = shift;

	(keys %$hash)[0];
}


my $keyword = {};

my $foo = {};
foreach my $x (keys %$city) {
	foreach my $ct (keys %{$city->{$x}}) {
		if($city->{$x}->{$ct} > 250000) {
			if(defined($foo->{$ct}->{$x})) {
				if($foo->{$ct}->{$x} < $city->{$x}->{$ct}) {
					$foo->{$ct}->{$x} = $city->{$x}->{$ct};
				}
			} else {
				$foo->{$ct}->{$x} = $city->{$x}->{$ct};
			}
		}
	}
}
foreach my $ct (keys %$foo) {
	$foo->{$ct} = [sort {$foo->{$ct}->{$b} <=> $foo->{$ct}->{$a}} keys %{$foo->{$ct}}];
	# if(len($foo->{$ct}) > 20) {
	# 	$foo->{$ct} = subset($foo->{$ct}, seq(0, 19));
	# }

	foreach my $y (@{$foo->{$ct}}) {
		$keyword->{$y} = $ct;
	}
}

foreach my $x (keys %$usa_states) {
	$keyword->{$x} = $usa_states->{$x};
}

foreach my $x (keys %$official) {
	if($x!~/\s/) {
		$keyword->{$x} = $official->{$x};
	}
}


foreach my $x (keys %$state) {
	if($x!~/\s/ and length($x) >= 5) {
		if(len($state->{$x}) == 1) {
			$keyword->{$x} = first_key($state->{$x});
		}
	}
}

foreach my $x (keys %$province) {
	if($x!~/\s/ and length($x) >= 5) {
		if(len($province->{$x}) == 1) {
			$keyword->{$x} = first_key($province->{$x});
		}
	}
}

$keyword->{"Chinese"} = "China";
$keyword->{"Japanese"} = "Japan";
$keyword->{"Korean"} = "South Korea";
$keyword->{"German"} = "Germany";
$keyword->{"French"} = "France";
$keyword->{"Indian"} = "India";
$keyword->{"Australian"} = "Australia";
$keyword->{"British"} = "United Kingdom";
$keyword->{"Danish"} = "Denmark";
$keyword->{"Swedish"} = "Sweden";
$keyword->{"Russian"} = "Russia";
$keyword->{"Bayerisches"} = "Germany";
$keyword->{"Bavarian"} = "Germany";

$keyword->{"UCLA"} = "United States";
$keyword->{"Stanford"} = "United States";
$keyword->{"Hopkins"} = "United States";
$keyword->{"Cornell"} = "United States";
$keyword->{"Yale"} = "United States";
$keyword->{"Harvard"} = "United States";
$keyword->{"Kinki"} = "Japan";
$keyword->{"Ehime"} = "Japan";
$keyword->{"Kitasato"} = "Japan";
$keyword->{"Hokkaido"} = "Japan";
$keyword->{"Shinshu"} = "Japan";
$keyword->{"Liège"} = "Belgium";
$keyword->{"Nihon"} = "Japan";
$keyword->{"Keio"} = "Japan";
$keyword->{"Göteborg"} = "Sweden";
$keyword->{"Göteborgs"} = "Sweden";
$keyword->{"Tohoku"} = "Japan";
$keyword->{"Juntendo"} = "Japan";
$keyword->{"Toho"} = "Japan";
$keyword->{"Rockefeller"} = "United States";
$keyword->{"Gunma"} = "Japan";
$keyword->{"Toranomon"} = "Japan";
$keyword->{"Kyorin"} = "Japan";
$keyword->{"Fujita"} = "Japan";
$keyword->{"Nishijin"} = "Japan";
$keyword->{"Mikatahara"} = "Japan";
$keyword->{"Matsushita"} = "Japan";
$keyword->{"Teikyo"} = "Japan";
$keyword->{"Kansai"} = "Japan";
$keyword->{"Kanagawa"} = "Japan";
$keyword->{"Iwate"} = "Japan";
$keyword->{"Osaki"} = "Japan";
$keyword->{"Jichi"} = "Japan";
$keyword->{"Shimane"} = "Japan";
$keyword->{"Wadayama"} = "Japan";
$keyword->{"Kyushu"} = "Japan";
$keyword->{"Kitano"} = "Japan";
$keyword->{"Dokkyo"} = "Japan";
$keyword->{"Aichi"} = "Japan";
$keyword->{"Chibanishi"} = "Japan";
$keyword->{"Sakakibara"} = "Japan";
$keyword->{"Kindai"} = "Japan";
$keyword->{"Mitsui"} = "Japan";
$keyword->{"Nippon"} = "Japan";
$keyword->{"Yoshijima"} = "Japan";
$keyword->{"Kokura"} = "Japan";
$keyword->{"Vanderbilt"} = "United States";
$keyword->{"Kanagawa"} = "Japan";
$keyword->{"Duke"} = "United States";
$keyword->{"Yonsei"} = "South Korea";
$keyword->{"Northwestern"} = "United States";
$keyword->{"Karolinska"} = "Sweden";
$keyword->{"Sumitomo"} = "Japan";
$keyword->{"Tenri"} = "Japan";
$keyword->{"Sungkyunkwan"} = "Japan";
$keyword->{"Amtssygehuset"} = "Denmark";
$keyword->{"Princeton"} = "United States";
$keyword->{"UCSF"} = "United States";
$keyword->{"Sechenov"} = "Russia";
$keyword->{"Oddziału"} = "Poland";
$keyword->{"Akademii"} = "Poland";
$keyword->{"Gruźlicy"} = "Poland";
$keyword->{"Weizmann"} = "Israel";
$keyword->{"Coruña"} = "Spain";
$keyword->{"NCI"} = "United States";
$keyword->{"NIH"} = "United States";
$keyword->{"Caltech"} = "United States";
$keyword->{"Taoyuan"} = "Taiwan";
$keyword->{"MIT"} = "Taiwan";
$keyword->{"Max-Planck-Institute"} = "Germany";
$keyword->{"Tel-Aviv"} = "Israel";
$keyword->{"Leuven"} = "Belgium";
$keyword->{"Hamburg-Eppendorf"} = "Belgium";
$keyword->{"Lausanne"} = "Switzerland";
$keyword->{"Leibniz-Institute"} = "Germany";
$keyword->{"Yat-Sen"} = "China";
$keyword->{"Jülich"} = "Germany";

$dict->{"keyword"} = $keyword;


my $keywords_phrase = {};
$keywords_phrase->{"Mexico"} = "New Mexico";
$keywords_phrase->{"Wales"} = "New South Wales";
$keywords_phrase->{"Ireland"} = "Northern Ireland";
$keywords_phrase->{"York"} = "New York";
$keywords_phrase->{"Puerto"} = "Puerto Rico";
$keywords_phrase->{"Mongolia"} = "Inner Mongolia";
$keywords_phrase->{"United"} = "United States";
$keywords_phrase->{"Max"} = "Max Planck Institute";

foreach my $x (keys %$uni) {
	if($x =~/^(\S+) University$/) {
		$keywords_phrase->{$1} = $x;
	}
}
delete($keywords_phrase->{"Atlantic"});

my $keyword_phrase_to_remove = {};

$keyword_phrase_to_remove->{"England"}->{"New England"} = 1;
$keyword_phrase_to_remove->{"Denmark"}->{"Denmark Hill"} = 1;
$keyword_phrase_to_remove->{"British"}->{"British Columbia"} = 1;
$keyword_phrase_to_remove->{"Wales"}->{"Prince of Wales"} = 1;
$keyword_phrase_to_remove->{"Israel"}->{"Beth Israel"} = 1;
$keyword_phrase_to_remove->{"France"}->{"Little France"} = 1;
$keyword_phrase_to_remove->{"Brasil"}->{"Ave Brasil"} = 1;
$keyword_phrase_to_remove->{"Madagascar"}->{"Madagascar Department"} = 1;
$keyword_phrase_to_remove->{"Panama"}->{"Panama Street"} = 1;
$keyword_phrase_to_remove->{"Paraguay"}->{"Diagonal Paraguay"} = 1;
$keyword_phrase_to_remove->{"Swedish"}->{"Swedish Medical Center"} = 1;
$keyword_phrase_to_remove->{"Swedish"}->{"Swedish Neuroscience Institute"} = 1;
$keyword_phrase_to_remove->{"Guinea"}->{"Guinea Worm"} = 1;
$keyword_phrase_to_remove->{"Italia"}->{"Italia Av"} = 1;

$dict->{"keywords_phrase"} = $keywords_phrase;
$dict->{"keyword_phrase_to_remove"} = $keyword_phrase_to_remove;

sub map_to_keyword {
	my $name = shift;
	my $dict = shift;
	my $array = shift;

	if(defined($dict->{"keyword_phrase_to_remove"}->{$name})) {
		my $ind = which(test($array, sub {$_[0] eq $name}));
		if($ind > 0) {
			$ind = $ind->[0];
			my $w = $name;
			for(my $i = $ind-1; $i >= 0; $i --) {
				$w = "$array->[$i] $w";
				if(defined($dict->{"keyword_phrase_to_remove"}->{$name}->{$w})) {
					return ("", "");
				}
			}

			$w = $name;
			for(my $i = $ind+1; $i <= $#$array; $i ++) {
				$w = "$w $array->[$i]";

				if(defined($dict->{"keyword_phrase_to_remove"}->{$name}->{$w})) {
					return ("", "");
				}
			}
		}
	}

	if(defined($dict->{"keywords_phrase"}->{$name})) {
		my $ind = which(test($array, sub {$_[0] eq $name}));
		if($ind > 0) {
			$ind = $ind->[0];
			my $w = $name;
			my $v;
			for(my $i = $ind-1; $i >= 0; $i --) {
				$w = "$array->[$i] $w";

				$v = map_to_country_name($w, $dict);
				if($v ne "") {
					return ($v, "country");
				}

				$v = map_to_uni_name($w, $dict);
				if(defined($v)) {
					if(len($v) == 1) {
						return (first_key($v), "uni");
					}
				}

				$v = map_to_province($w, $dict);
				if(defined($v)) {
					if(len($v) == 1) {
						return (first_key($v), "province");
					}
				}
 			}

 			$w = $name;
 			for(my $i = $ind+1; $i <= $#$array; $i ++) {
				$w = "$w $array->[$i]";

				$v = map_to_country_name($w, $dict);
				if($v ne "") {
					return ($v, "country");
				}

				$v = map_to_uni_name($w, $dict);
				if(defined($v)) {
					if(len($v) == 1) {
						return (first_key($v), "uni");
					}
				}

				$v = map_to_province($w, $dict);
				if(defined($v)) {
					if(len($v) == 1) {
						return (first_key($v), "province");
					}
				}
 			}
		}
	}

	if(defined($dict->{"exclude"}->{$name})) {
		return ("", "");
	}

	my $v = map_to_country_name($name, $dict);
	if($v ne "") {
		return ($v, "country");
	}

	if(defined($dict->{"keyword"}->{$name})) {
		return ($dict->{"keyword"}->{$name}, "keyword");
	} else {
		return ("", "");
	}
}

# return a hash
sub map_to_city_name {
	my $name = shift;
	my $dict = shift;

	if(defined($dict->{"exclude"}->{$name})) {
		return undef;
	}

	my $city = $dict->{"city"};
	my $city_alt = $dict->{"city_alt"};

	my $h = {};
	if(defined($city->{$name})) {
		$h = $city->{$name};

	} 

	if(defined($city_alt->{$name})) {
		if(len($city_alt->{$name}) == 1) {
			my $h2 = first_value($city_alt->{$name});

			foreach my $x (keys %$h2) {
				if(defined($h->{$x})) {
					if($h->{$x} < $h2->{$x}) {
						$h->{$x} = $h2->{$x};
					}
				} else {
					$h->{$x} = $h2->{$x};
				}
			}
		}
	}

	if(len($h)) {
		return $h;
	} else {
		return undef;
	}
}


sub map_to_uni_name {
	my $name = shift;
	my $dict = shift;

	if(defined($dict->{"exclude"}->{$name})) {
		return undef
	}

	my $uni = $dict->{"uni"};
	my $uni_alt = $dict->{"uni_alt"};

	my $hh = {};


	if(defined($uni->{$name})) {
		$hh = copy(first_value($uni->{$name}));
		my $cc2 = first_key($uni->{$name});
		my $ct = first_key($hh);

		if(defined($dict->{"city"}->{$cc2}->{$ct})) {
			$hh->{$ct} = $dict->{"city"}->{$cc2}->{$ct};
		} else {
			$hh->{$ct} = 10000;
		}
	}

	if(defined($uni_alt->{$name})) {
		foreach my $x (keys %{$uni_alt->{$name}}) {
			my $cc = first_key( $uni_alt->{$name}->{$x} );
			my $ct = first_key( $uni_alt->{$name}->{$x}->{$cc} );

			my $cc2 = normalize_city_name($cc, $ct, $dict->{"city"}, $dict->{"city_alt"});

			if($cc2 ne "") {
				$hh->{$ct} = $dict->{"city"}->{$cc2}->{$ct};
			} else {
				$hh->{$ct} = 10000;
			}
		}
	}

	if(len($hh)) {
		return $hh;
	} else {
		return undef;
	}
}


my $exclude = {};
$exclude->{"Centre"} = 1;

$dict->{"exclude"} = $exclude;

my $count = {};
my $cache = {};


# priority:
# - $official: strings that can be directedly mapped to official country names
# - $usa_states: US states names
# - $city
# - $uni
# - email domain names
sub process {
	my $file = shift;
	my $dict = shift;
	my $count = shift;
	my $cache = shift;

	print "$file\n";

	open F, "/Volumes/WD/pubmed_formatted/$file" or die;
	my $output = $file;
	$output =~s/formatted/normalized/;
	open OUT, ">/Volumes/WD/pubmed_formatted/$output" or die "cannot create $output";


	my $line = <F>;
	print OUT "pmid	journal	journal_abbr	journal_issn	journal_country	journal_uid	language	pub_year	n_authors	affiliation	references	country\n";


	my $i = 0;
	my $verbose = 1;

	while($line = <F>) {

		$i ++;

		$count->{paper_total} ++;


		$line =~s/\n$//;
		if($line !~/^\d+/) {
			$count->{paper_no_pmid} ++;
			next;
		}
		my @line = split "\t", $line, -1;

		if($line[7] eq "NA") {  # has pub_year
			$count->{paper_no_pub_year} ++;
			next;
		}
		
		# only 2000 ~ 2023
		if($line[7] < 2000 or $line[7] >= 2024) {
			$count->{paper_not_in_year_range} ++;
			next;
		}

		# only english
		if($line[6] ne "eng") {
			$count->{paper_no_english} ++;
			next;
		}
		# n_authors <= 50
		if($line[8] > 50 or $line[8] == 0) {
			$count->{paper_not_in_author_range} ++;
			next;
		}

		if($line[5] eq "") {  # journal has a uid
			$count->{paper_no_journal_uid} ++;
			next;
		}

		$verbose and print "======= $line[0] (n_authors = $line[8]) ========\n";
		
		my @af1 = split_affiliation($line[9]);

		if(scalar @af1 == 0) {  # has (one or multiple) authors, but no afflications
			$count->{paper_no_affiliation} ++;
			# $verbose and print STDERR "  no affiliation\n";

			next;
		}

		$count->{paper_include} ++;

		## special cases, The New England journal of medicine
		if($line[5] eq "0255562") {
			if(len(unique(\@af1)) == 1) {  # all athors share the same affiliations
				@af1 = ($af1[0]);
			}
		}

		my @co1 = ();

		for(my $k = 0; $k <= $#af1; $k ++) {
			my @af2;

			if($line[5] eq "0255562" and scalar @af1 == 1) {
				@af2 = @af1;
			} else {
				@af2 = split ' %%%% |; ', $af1[$k];  # an author has multiple affiliations
				@af2 = grep {$_ ne ""} @af2;
			}

			my $v;
			if(scalar @af1 == 1 and scalar @af2 == 1 and length($af2[0]) > 60 and $line[8] > 1) {
				$v = detect_country_in_single_text($af2[0], $dict, $cache, 0.8);
				# print STDERR join ",", @co1; print STDERR "\t$af1[0]\n\n";
				if($v eq "_none_") {
					$count->{af_from_single_text_not_matched} ++;
					$co1[0] = "";
					$verbose and print STDERR "$file\t$line[0]\tsingle_text_none\t_NA_\t0\t$af2[0]\n";
				} elsif($v eq "_multiple_") {
					$count->{af_from_single_text_multiple} ++;
					$co1[0] = "";
					$verbose and print STDERR "$file\t$line[0]\tsingle_text_multiple\t_NA_\t0\t$af2[0]\n";
				} else {
					$count->{af_from_single_text_matched} ++;
					$co1[0] = $v;
					# $verbose and print STDERR "$line[0]\tsingle_text\t".join(",", @co1)."\t1\t$af2[0]\n";
				}
			} else {
				my @co2  = ();
				for(my $j = 0; $j <= $#af2; $j ++) {
					$v = detect_country_in_single_text($af2[$j], $dict, $cache);

					if($v eq "_none_") {
						$count->{af_from_guess_country_not_matched} ++;
						$co2[$j] = "";
						$verbose and print STDERR "$file\t$line[0]\tguess_country_none\t_NA_\t0\t$af2[$j]\n";
					} elsif($v eq "_multiple_") {
						$count->{af_from_guess_country_multiple} ++;
						$co2[$j] = "";
						$verbose and print STDERR "$file\t$line[0]\tguess_country_multiple\t_NA_\t0\t$af2[$j]\n";
					} else {
						$count->{af_from_guess_country_matched} ++;
						$co2[$j] = $v;
						# $verbose and print STDERR "$line[0]\tguess_country\t$co2[$j]\t$ss->{$co2[$j]}\t$af2[$j]\n";
					}
				}
				@co2 = grep {$_ ne ""} @co2;
				$co1[$k] = join "|", @co2;
			}
		}
		push(@line, join ";", @co1);

		print OUT join "\t", @line; print OUT "\n";
	}
	close OUT;
}

sub split_affiliation {
	my $txt = shift;

	my @af1 = split ' \|\|\|\| ', $txt;
	@af1 = grep {$_ ne ""} @af1;

	if(scalar @af1 == 1) {
		
		@af1 = split "; *|†|‡|§", $af1[0];
		if(scalar @af1 == 1) {
			$af1[0] =~s/[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]+)\b//g;
			@af1 = split '(?<=[a-z])(?<=[^,]{4})\. (?=[A-Z])', $af1[0];
		} else {
			@af1 = map {$_=~s/\s*\(.*\)//g;$_;} @af1;
		}
	}


	return @af1;
}

sub scale {
	my $hash = shift;
	my $factor = shift;

	if(!defined($factor)) {
		$factor = 1;
	}

	my $sum = 0;
	foreach my $x (keys %$hash) {
		$sum += $hash->{$x};
	}
	my $hash2;
	foreach my $x (keys %$hash) {
		$hash2->{$x} = $hash->{$x}/$sum * $factor;
	}

	return $hash2;
}

sub detect_country_in_single_text {
	my $text = shift;
	my $dict = shift;
	my $cache = shift;
	my $p = shift or 0.6;

	$text =~s/\s*\.\s*$//;

	if(defined($cache->{$text})) {
		return $cache->{$text};
	}

	my @words = (split /[,+:;+()]|\. /, $text);
	@words = grep {$_!~/^\s*$/} @words;
	@words = map {$_=~s/^\s+|\s+$//g; $_} @words;
	@words = map {$_=~s/^[[:punct:]]+|[[:punct:]]+$//g; $_} @words;
	@words = grep {$_!~/^\s*$/} @words;

	my $score = [];
	my $v;
	for(my $i = 0; $i <= $#words; $i ++) {
		my $x = $words[$i];
		$score->[$i] = {};

		$v = map_to_country_name($x, $dict);
		if($v ne "") {
			if($i == $#words) {
				$score->[$i]->{$v} = 100;
			} else {
				$score->[$i]->{$v} = 10;
			}
			next;
		}

		$v = map_to_uni_name($x, $dict);
		if(defined($v)) {
			$score->[$i] = scale($v, 5);
			next;
		} else {
			if($x !~/^The/) {
				$v = map_to_uni_name("The $x", $dict);
				if(defined($v)) {
					$score->[$i] = scale($v, 5);
					next;
				}
			} else {
				my $x2 = $x;
				$x2 =~s/^The //;
				$v = map_to_uni_name($x2, $dict);
				if(defined($v)) {
					$score->[$i] = scale($v, 5);
					next;
				}
			}
		}

		$v = map_to_usa_states($x, $dict);
		if($v ne "") {
			$score->[$i]->{$v} = 10;
			next;
		}

		$v = map_to_domain_name($x, $dict);
		if($v ne "") {
			$score->[$i]->{$v} = 1;
			next;
		}

		$v = map_to_province($x, $dict);
		if(defined($v)) {
			$score->[$i] = scale($v);
			next;
		}

		$v = map_to_city_name($x, $dict);
		if(defined($v)) {
			$score->[$i] = scale($v);
			next;
		}

		my @fw = split " ", $x;
		my $bar;
		if(scalar @fw > 1) {
			for(my $k = 0; $k <= $#fw; $k ++) {
				if($fw[$k] eq "USA") {
					$score->[$i]->{"United States"} = +10;
				}
				if($fw[$k] eq "UK") {
					$score->[$i]->{"United Kingdom"} = +10;
				}
				if($fw[$k] eq "US") {
					$score->[$i]->{"United States"} = +10;
				}
				if(length($fw[$k]) < 4) {
					next;
				}

				($v, $bar) = map_to_keyword($fw[$k], $dict, \@fw);
				if($v ne "") {
					if($bar eq "country") {
						$score->[$i]->{$v} = +10;
					} else {
						$score->[$i]->{$v} = +1;
					}
				}
			}
		}
	}

	my $sum = {};

	foreach my $array (@$score) {
		foreach my $x (keys %$array) {
			$sum->{$x} += $array->{$x};
		}
	}

	if(len($sum) == 0) {
		if($text =~/Turkey/) {
			$sum->{"Turkey"} = 1;
		}
	}

	if(len($sum) == 0) {
		$cache->{$text} = "_none_";
		return "_none_";
	}

	if(defined($sum->{"Hong Kong"}) and defined($sum->{"China"})) {
		delete($sum->{"China"});
	}
	if(defined($sum->{"Macau"}) and defined($sum->{"China"})) {
		delete($sum->{"China"});
	}
	if(defined($sum->{"Taiwan"}) and defined($sum->{"China"}) and $sum->{"China"}/$sum->{"Taiwan"} < 5) {
		delete($sum->{"China"});
	}
	if(defined($sum->{"Lebanon"}) and defined($sum->{"United States"})) {
		delete($sum->{"Lebanon"});
	}
	if(defined($sum->{"Martinique"}) and defined($sum->{"France"})) {
		delete($sum->{"France"});
	}
	if(defined($sum->{"Reunion"}) and defined($sum->{"France"})) {
		delete($sum->{"France"});
	}
	if(defined($sum->{"Jamaica"}) and defined($sum->{"United States"})) {
		delete($sum->{"United States"});
	}

	$v = [values %$sum];
	if(max($v) / sum($v) >= $p) {
		$cache->{$text} = which_max_hash($sum);
		return $cache->{$text};
	} else {
		# print Dumper(@words);
		# print Dumper($score);
		# print Dumper($sum);
		$cache->{$text} = "_multiple_";
		return "_multiple_";
	}
}


sub map_to_domain_name {
	my $name = shift;
	my $dict = shift;

	# email domain name
	if($name =~/[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.([A-Za-z]+)\b/) {
		my $d = uc($1);
		if($d eq "EDU" or $d eq "GOV") {
			return "United States";
		} elsif($d eq "UK") {
			return "United Kingdom";
		} else {
			if(defined($dict->{"domain"}->{$d})) {
				return $dict->{"domain"}->{$d};
			}
		}

		$name =~/[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]+)\b/;
		if($1 eq "163.com" or $1 eq "sina.com" or $1 eq "126.com") {
			return "China";
		}
	}

	return "";
}

sub regexpr_to_keyword {
	my $text = shift;
	my $dict = shift;
	my $score = shift;

	my $official = $dict->{"official"};
	my $usa_states = $dict->{"usa_states"};
	my $keyword = $dict->{"keyword"};

	foreach my $x (keys %$official) {
		if($text =~/\b$x\b/i) {
			$score->{ $official->{$x} } += 1000;
		}
	}

	foreach my $x (keys %$usa_states) {
		if($text =~/\b$x\b/i) {
			$score->{ $usa_states->{$x} } += 500;
		}
	}

	foreach my $x (keys %$keyword) {
		if($text =~/\b$x\b/i) {
			$score->{ $usa_states->{$x} } += 300;
		}
	}
}


for(my $i = $start; $i <= $end; $i ++) {
	if($i < 10) {
		process("pubmed24n000$i.formatted.tab", $dict, $count, $cache);
	} elsif($i < 100) {
		process("pubmed24n00$i.formatted.tab", $dict, $count, $cache);
	} elsif($i < 1000) {
		process("pubmed24n0$i.formatted.tab", $dict, $count, $cache);
	} else {
		process("pubmed24n$i.formatted.tab", $dict, $count, $cache);
	}
}


open STAT, ">processed/paper_stat1.txt" or die;
foreach my $x (keys %$count) {
	print STAT "$x\t$count->{$x}\n";
}
close STAT;


## $cache is mainly for validation

open VALIDATION, ">validation.txt" or die;
foreach my $x (keys %$cache) {
	if(rand(1) < 0.0001) {
		print VALIDATION "$cache->{$x}\t$x\n"
	}
}
close VALIDATION;

