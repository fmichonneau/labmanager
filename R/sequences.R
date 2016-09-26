
read_seq <- function(x, seq_path) {
    ff <- file.path(seq_path, x)
    if (! file.exists(ff))
        stop(sQuote(ff), " doesn't exist.")
    res <- readLines(file.path(seq_path, x))
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
