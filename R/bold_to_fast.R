bold_to_data_frame <- function(bold_xml) {
    seq <- xml_find_all(bold_xml, ".//sequences/sequence[markercode='COI-5P']/nucleotides[1]")
    tax <- xml_find_all(bold_xml, ".//sequences/sequence[markercode='COI-5P']/../../taxonomy")
    bin <- xml_find_all(bold_xml, ".//sequences/sequence[markercode='COI-5P']/../../bin_uri")
    rid <- xml_find_all(bold_xml, ".//sequences/sequence[markercode='COI-5P']/../../record_id")
    tax_info <- lapply(tax, function(x) {
        tax_lvl <- xml_name(xml_children(x))
        tax_lvl <- tax_lvl[tax_lvl %in%  c("phylum", "class", "order", "family", "genus", "species")]
        pth_str <- paste(tax_lvl, "taxon", "name", sep = "/")
        sapply(pth_str, function(y) xml_text(xml_find_all(x, y)))
    })
    tax_parsed <- vapply(tax_info, function(x) {
        paste(gsub("(.+)/(.+)/(.+)", "\\1", names(x)), x, sep = ": ", collapse = ", ")
    }, character(1))
    res <- data.frame(record_id = xml_text(rid),
                      BIN = xml_text(bin),
                      taxonomy = tax_parsed,
                      sequence = xml_text(seq),
                      stringsAsFactors = FALSE)
    res[["BIN"]][!nzchar(res[["BIN"]])] <- paste("noBIN", res[["record_id"]][!nzchar(res[["BIN"]])], sep = "-")
    res
}

bold_to_sap_fasta <- function(bold_rds = readRDS(file = "20160427-bold_sequence_dataframe.rds"),
                              out = "20160427-bold_sequences.fasta") {
    res <- apply(bold_rds, 1, function(x) {
        paste0(">", x[1], " ; ", x[3], " ; ", x[2], "\n", x[4])
    })
    cat(res, sep = "", file = out)
}
