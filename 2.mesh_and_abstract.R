library(GetoptLong)
library(xml2)
setwd("/Volumes/WD/pubmed")

xml_files = list.files(pattern = "gz$")
nf = length(xml_files)

start = 1
end = 1366

if(file.exists("~/project/analysis/citation_analysis/processed/pmid_to_mesh.tab")) {
	file.remove("~/project/analysis/citation_analysis/processed/pmid_to_mesh.tab")
}
con = file("~/project/analysis/citation_analysis/processed/pmid_to_mesh.tab", open = "a")

for(k in seq(start, end)) {

	cat(strrep("\b", 100))
	cat(k, "/", nf, "...")
		
	f = xml_files[k]
	xml = read_xml(f)
	articles = xml_find_all(xml, "PubmedArticle")

	pmid = xml_find_first(articles, "MedlineCitation/PMID") |> xml_text()
	
	meshes = xml_find_all(articles, "MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN='Y']", flatten = FALSE)
	mesh_terms = sapply(meshes, function(x) {
		paste(unique(xml_text(x)), collapse = ";")
	})

	df = data.frame(pmid = pmid, mesh = mesh_terms)
	df = df[grepl("^\\d+", pmid), , drop = FALSE]
	df = df[df$mesh != "", ]
	write.table(df, con, quote = FALSE, row.names = FALSE, sep = "\t", col.names = FALSE)
}

close(con)



for(k in seq(start, end)) {

	cat(strrep("\b", 100))
	cat(k, "/", nf, "...")
		
	f = xml_files[k]
	xml = read_xml(f)
	articles = xml_find_all(xml, "PubmedArticle")

	pmid = xml_find_first(articles, "MedlineCitation/PMID") |> xml_text()
	
	abstract = xml_find_all(articles, "MedlineCitation/Article/Abstract", flatten = FALSE)
	abstract = sapply(abstract, function(x) {
		if(length(x) == 0) {
			return("")
		} else {
			xml_text(x)
		}
	})
	abstract = gsub("<.*?>", "", abstract)
	abstract = gsub("\n", " ", abstract)

	df = data.frame(pmid = pmid, abstract = abstract)
	df = df[grepl("^\\d+", pmid), , drop = FALSE]
	df = df[df$abstract != "", ]
	write.table(df, file = paste0("../pubmed_formatted/", gsub("xml.gz$", "pmid_to_abstract.tab", f)), quote = FALSE, row.names = FALSE, sep = "\t", col.names = FALSE)
}


