##' @importFrom chopper mergeAlignment
align_by_phyla <- function(seq_path = "~/Documents/plankton-larvae-data/seqs/COI",
                           out_path = "/tmp",
                           sample_data = get_lab("sample_data"),
                           seq_pattern = NULL, phyla = NULL) {
    if (is.null(seq_pattern)) {
        seqs <- list.files(path = seq_path)
    } else {
        seqs <- list.files(path = seq_path, pattern = seq_pattern)
    }

    if (is.null(phyla)) {
        phyla <- unique(sample_data[["phylum"]])
    } else {
        if (! all(phyla %in% sample_data[["phylum"]])) {
            stop("Some of the specified phyla are not found in the data.")
        }
    }

    split_phyla <- split(sample_data, sample_data[["phylum"]])[phyla]

    seqs_phyla <- lapply(split_phyla, function(phyla_) {
        intersect(seqs, phyla_[["voucher_number"]])
    })
    stopifnot(length(unlist(seqs_phyla)) == length(seqs))
    seqs_phyla <- seqs_phyla[sapply(seqs_phyla, length) > 0]

    sapply(seq_along(seqs_phyla), function(x)
        chopper::mergeAlignment(seqs_phyla[[x]],
                                output = file.path(out_path, names(seqs_phyla)[x]),
                                seqFolder = seq_path
                                )
        )
}

##' @importFrom assertthat assert_that
##' @importFrom chopper removeFastaComments splitAlignment
import_sequence_to_repository <- function(file,
                                          seq_path = "~/Documents/plankton-larvae-data/seqs/COI",
                                          ...) {

    assertthat::assert_that(file.exists(file))
    chopper::removeFastaComments(file, output = NULL)
    chopper::splitAlignment(file = file, output = seq_path, format = "fasta")
}
