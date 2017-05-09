list_phylum <- function(sample_data = get_lab("sample_data")) {
    unique(sample_data[["phylum"]])
}

check_phylum <- function(phylum, sample_data = get_lab("sample_data")) {
    phylum_check <- phylum %in% list_phylum(sample_data)
    if (! all(phylum_check))
        stop(paste(sQuote(unique(phylum[! phylum_check])), collapse = ", "),
             " not valid", call. = FALSE)
}

check_voucher_number <- function(ids, sample_data = get_lab("sample_data")) {
    ids_check <- ids %in% sample_data[["voucher_number"]]
    if (! all(ids_check)) {
        stop(paste(sQuote(ids[! ids_check]), collapse = ", "),
             " not found", call. = FALSE)
    }
}
