library(GetoptLong)
library(xml2)
setwd("/Volumes/WD/pubmed")

xml_files = list.files(pattern = "gz$")
nf = length(xml_files)


start = 1
end = 1366
GetOptions(
	"start=i", "start index",
	"end=i", "end index"
)

for(k in seq(start, end)) {

	cat(strrep("\b", 100))
	cat(k, "/", nf, "...")
		
	f = xml_files[k]
	xml = read_xml(f)
	articles = xml_find_all(xml, "PubmedArticle")

	pmid = xml_find_first(articles, "MedlineCitation/PMID") |> xml_text()
	journal = xml_find_first(articles, "MedlineCitation/Article/Journal/Title") |> xml_text()
	journal_abbr = xml_find_first(articles, "MedlineCitation/Article/Journal/ISOAbbreviation") |> xml_text()
	language = xml_find_first(articles, "MedlineCitation/Article/Language") |> xml_text()

	# print ISSN and/or electronic ISSN
	journal_issn1 = xml_find_first(articles, "MedlineCitation/MedlineJournalInfo/ISSNLinking") |> xml_text()
	journal_issn2 = xml_find_first(articles, "MedlineCitation/Article/Journal/ISSN[@IssnType='Electronic']") |> xml_text()
	journal_issn3 = xml_find_first(articles, "MedlineCitation/Article/Journal/ISSN[@IssnType='Print']") |> xml_text()

	issn = cbind(journal_issn1, journal_issn2, journal_issn3)
	journal_issn = apply(issn, 1, function(x) {
		paste(unique(x[!is.na(x)]), collapse = ",")
	})

	journal_country = xml_find_first(articles, "MedlineCitation/MedlineJournalInfo/Country") |> xml_text()
	journal_uid = xml_find_first(articles, "MedlineCitation/MedlineJournalInfo/NlmUniqueID") |> xml_text()
	
	# year may be in different places and formats
	pub_year = xml_find_first(articles, "MedlineCitation/Article/Journal/JournalIssue/PubDate/Year") |> xml_text()
	pub_year2 = xml_find_first(articles, "MedlineCitation/Article/Journal/JournalIssue/PubDate/MedlineDate") |> xml_text()
	pub_year2[!grepl("^(\\d\\d\\d\\d)", pub_year2)] = NA
	pub_year2 = gsub("^(\\d\\d\\d\\d).*$", "\\1", pub_year2)

	l = is.na(pub_year)
	pub_year[l] = pub_year2[l]

	pub_year3 = xml_find_first(articles, "MedlineCitation/Article/ArticleDate/Year") |> xml_text()
	l = is.na(pub_year)
	pub_year[l] = pub_year3[l]
		
	# for a single author, multiple affilications are separated by " %%%% "
	# for a paper, multiple authors are separated by " |||| "
	authors = xml_find_all(articles, "MedlineCitation/Article/AuthorList/Author[descendant::LastName]", flatten = FALSE)
	n_authors = sapply(authors, length)
	paper_id = rep(seq_along(n_authors), times = n_authors)

	authors = xml_find_all(articles, "MedlineCitation/Article/AuthorList/Author[descendant::LastName]", flatten = TRUE)
	af = sapply(xml_find_all(authors, "AffiliationInfo/Affiliation", flatten = FALSE), function(x) {
		paste(unlist(strsplit(xml_text(x), ";")), collapse = " %%%% ") # a single affiliation text for a single author can still contain mutliple affilications
	})
	af = sapply(split(af, paper_id), paste, collapse = " |||| ")
	affiliations = rep("", length(pmid))
	names(affiliations) = seq_along(pmid)

	affiliations[names(af)] = af
	affiliations = unname(affiliations)
	affiliations = gsub("\n", " ", affiliations)
	
	ref = xml_find_all(articles, "PubmedData/ReferenceList/Reference/ArticleIdList/ArticleId[@IdType='pubmed']", flatten = FALSE)
	ref = sapply(ref, function(x) {
		paste(xml_text(x), collapse = ";")
	})
	
	df = data.frame(pmid = pmid, journal = journal, journal_abbr = journal_abbr, journal_issn = journal_issn,
		journal_country = journal_country, journal_uid = journal_uid, language = language,
		pub_year = pub_year, n_authors = n_authors, affiliation = affiliations,
		references = ref)
	df = df[grepl("^\\d+", pmid), , drop = FALSE]
	write.table(df, file = paste0("../pubmed_formatted/", gsub("xml.gz$", "formatted.tab", f)), quote = FALSE, row.names = FALSE, sep = "\t")
}



