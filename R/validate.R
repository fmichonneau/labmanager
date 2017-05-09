validate_lab <- function(what, res) {
    if (identical(what, "sequencing_plate_data")) {
        ## no spaces in taxa names
        tt <- sapply(c("bold_phylum_id", "bold_order_id", "bold_family_id", "bold_genus_id", "bold_species_id"),
                     function(x) {
            with_spaces <- sapply(res[[x]], nchar)
            no_spaces <- sapply(res[[x]], function(i) nchar(gsub("\\s", "", i)))
            if (any(na.omit(with_spaces) != na.omit(no_spaces)))
                stop("there are spaces in the taxa names of ", what)
        })
    }
}


check_voucher_extractions <- function(ext = get_lab("extraction_data"),
                                      samp = get_lab("sample_data")) {
    miss_samp <- ext$voucher_number %in% samp$voucher_number
    if (!all(miss_samp)) {
        miss <- ext$voucher_number[!miss_samp]
        stop("Some voucher in extractions are not in sample_data: ",
             miss)
    }
}

validate_sequences <- function(seq_path = "~/Documents/plankton-larvae-data/seqs/COI") {
    ## let's make sure that the names of the sequences match the file names
    seqs <- list_sequences(seq_path = seq_path)
    res <- lapply(seqs, read_seq, seq_path) %>% dplyr::bind_rows()
    if (!all(seqs == res$voucher_number)) {
        stop("Sequence names don't match file names for files: ",
             paste(seqs[seqs != res$voucher_number]))
    }
}
