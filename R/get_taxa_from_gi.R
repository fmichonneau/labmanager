store_ncbi_taxonomy <- function(path = "ncbi_taxonomy_store") {
    invisible(storr::storr_external(storr::driver_rds(path),
                                    hook_get_taxonomy_from_gid))
}

hook_get_taxonomy_from_gid <- function(key, namespace) {
    res <- rentrez::entrez_link(dbfrom = "nucleotide", id = key, db = "taxonomy")
    if (length(res$links) == 1L) {
        rentrez::entrez_fetch("taxonomy", id = res$links[[1]], rettype = "xml")
    } else stop("something is wrong with the id.")
}

get_taxonomy_from_gid <- function(gid) {
    ## use nucleotide namespace
    store_ncbi_taxonomy()$get(gid, "nucleotide")
}

## Create a 1 row data frame from the XML file returned by the NCBI
## taxonomy database.
parse_ncbi_xml <- function(xml_file) {
    xf <- xml_file %>%
        xml2::read_xml()

    species_name <- xf %>%
        xml2::xml_find_first(".//Taxon/ScientificName") %>%
        xml2::xml_text()

    nm <- xml_find_all(xf, ".//Taxon/LineageEx/Taxon/*") %>% xml_name() %>% unique()
    res <- as.vector(length(nm), "list")
    for (i in seq_along(nm)) {
        res[[i]] <- xml2::xml_find_all(xf, paste0(".//Taxon/LineageEx/Taxon/*[", i, "]/text()")) %>%
            xml2::xml_text()
    }
    names(res) <- nm
    res <- as_tibble(res)

    res_last <- dplyr::slice(res, n())

    col_names <- c("superkingdom", "kingdom",
                   "phylum", "subphylum", "class",
                   "order", "family", "genus", "species")

    RES <- res %>%
        dplyr::filter(Rank %in% col_names) %>%
        dplyr::bind_rows(res_last) %>%
        dplyr::distinct(.keep_all = TRUE) %>%
        dplyr::select(-TaxId) %>%
        tidyr::spread(Rank, ScientificName)

    if (exists("no rank", RES)) {
        RES <- RES %>%
            dplyr::rename(lowest = `no rank`)
    }
    RES
}

## put it together: From the blast output (formatted as a table),
## this:
## - extract the gi numbers
## - use the ncbi_store to get the taxonomic info from the gi number
## - parse the resulting XML into a data frame
## - add this data to the original data frame
get_taxa <- function(db_file = "12-assignment/20170515_blast_output.tbl") {
    db <- read.table(db_file, header = F, sep = "\t")
    if (ncol(db) == 12) {
        names(db) <- c("seq", "gb_number", "pident", "length",
                       "mismatch", "gapopen", "qstart", "qend",
                       "sstart", "ssend", "evalue", "bitscore")
    } else stop("can only deal with the standard 12 column output for now.")

    db_full <- tidyr::separate(db, col = gb_number,
                        into = c("gi", "gid", "gb", "gbd", "x"),
                        sep = "\\|") %>%
        select(-x)

    purrr::map_chr(db_full$gid, get_taxonomy_from_gid) %>%
        purrr::map_df(parse_ncbi_xml) %>%
        dplyr::bind_cols(db)
}
