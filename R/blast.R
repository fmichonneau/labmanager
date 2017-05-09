make_blast_db <- function(sample_esu = get_lab("sample_esu"),
                          out = file.path("~/Documents/plankton-larvae-data/taxonomic_assignment",
                                          paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),
                                                 "-plankton-sequences-blast.db"))) {

    ## using sample_esu here only retrieves phyla for which we have sequences for
    lst_phyla <- list_phylum(sample_esu)

    res <- get_sequences_by_phylum(lst_phyla) %>%
        dplyr::bind_rows(.id = "phylum")

    res$title <- paste0(res$phylum, "_", res$voucher_number)

    f <- res %>%
        dplyr::select(title, sequence) %>%
        purrr::pmap(prepare_fasta) %>%
        write_fasta(out)

    cmd <-  paste(
        "makeblastdb -in", f, "-dbtype nucl"
    )
    message(cmd)
    system(cmd)
}
