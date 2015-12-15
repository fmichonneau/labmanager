
put_ids_in_names <- function(seqs, path_seq = "~/Documents/plankton-larvae-data/seqs/COI/",
                             outfile) {

    seqs_ids <- get_lab("sequencing_plate_data")
    sample_ids <- get_lab("sample_data")

    all_seqs <- tempfile("sequences", fileext = ".fasta")
    chopper::mergeAlignment(listFiles = seqs, output = all_seqs,
                            seqFolder = path_seq)

    alg <- ape::read.dna(file = all_seqs, format = "fasta", as.matrix = TRUE)
    seq_lbl <- dimnames(alg)[[1]]

    ## add the BOLD ids to the labels
    seq_pos <- match(seq_lbl, seqs_ids[["voucher_number"]])
    new_lbl <- paste(seqs_ids[["bold_phylum_id"]][seq_pos],
                     seqs_ids[["bold_genus_id"]][seq_pos],
                     seqs_ids[["bold_species_id"]][seq_pos],
                     seqs_ids[["voucher_number"]][seq_pos],
                     sep = "_")
    new_lbl <- gsub("NA_", "", new_lbl)
    new_lbl <- gsub("_{2,}", "", new_lbl)

    ## add the phylum from the samples
    seq_pos <- match(seq_lbl, sample_ids[["voucher_number"]])
    new_lbl <- paste(abbreviate(sample_ids[["phylum"]][seq_pos], 3),
                     new_lbl, sep = "_")
    new_lbl <- gsub("_{2,}", "", new_lbl)

    ## replace the ids
    dimnames(alg)[[1]] <- new_lbl

    ape::write.dna(alg, file = outfile, format = "fasta")

}
