
list_sequences <- function(pattern = NULL, seq_path = "~/Documents/plankton-larvae-data/seqs/COI/") {
    list.files(path = seq_path, pattern = pattern)
}

read_seq <- function(x, seq_path) {
    ff <- file.path(seq_path, x)
    if (! file.exists(ff))
        stop(sQuote(ff), " doesn't exist.")
    res <- readLines(file.path(seq_path, x))
    stopifnot(identical(length(res), 2L))
    data.frame(voucher_number = gsub("^>", "", res[1]),
               sequence = res[2], stringsAsFactors = FALSE)
}

get_sequences_by_phylum <- function(phylum, sample_esu = get_lab("sample_esu"),
                                    seq_path = "~/Documents/plankton-larvae-data/seqs/COI/",
                                    fasta_file = NULL) {
    check_phylum(phylum, sample_esu)

    phylum_vchr <- get_voucher_numbers_by_phylum(phylum, sample_esu)

    seqs <- lapply(phylum_vchr, function(vchr) {
        res <- lapply(vchr, read_seq, seq_path)
        res <- dplyr::bind_rows(res)
        res$esu <- get_esu_by_voucher_number(res$voucher_number)
        res
    })

    if (is.null(fasta_file)) return(seqs)

    lapply(seqs, function(x) {
        res <- apply(x, 1, function(seq) {
            paste0(">", paste0(seq[3], "_", seq[1]), "\n",
                   seq[2])
        })
        cat(res, sep = "\n", file = fasta_file)
    })
    fasta_file
}

fetch_hook_bold_seq_id <- function(key, namespace) {
    stopifnot(!missing(namespace))
    seq_path <- "~/Documents/plankton-larvae-data/seqs/COI/"
    message("Looking for ",  key, " in the ", namespace, " BOLD database")
    seq <- read_seq(key, seq_path)$sequence
    res <- bold::bold_identify(sequence = seq)
    res[[1]]
}

store_bold_seq_id <- function(store_path = "~/Documents/plankton-larvae-data/bold_data_store") {
    storr::storr_external(storr::driver_rds(path = store_path, mangle_key = FALSE),
                          fetch_hook_bold_seq_id)
}

update_bold_seq_id_store <- function(seq_path = "~/Documents/plankton-larvae-data/seqs/COI") {
    all_seqs <- list.files(path = seq_path)
    all_seqs <- list_sequences()
    out <- lapply(all_seqs, function(x) {
        store_bold_seq_id()$get(x, namespace = "COX1")
        store_bold_seq_id()$get(x, namespace = "COX1_SPECIES")
    })
    invisible(out)
}
