##' Function to generate the oligos files needed by Mothur for
##' de-multiplexing
##'
##' @param run name of the sequencing run
##' @param data table with metadata info about the run
##' @param primers for now just leray for the usual COI leray primer
##'     combination
##' @param barcodes the table with the correspondance between the
##'     primer tag names and their respective sequences
##' @param outdir where the oligos files should be written
##'
##' @details Currently the function output oligo files with the same
##'     naming scheme as returned by ICBR but it might not be the
##'     best in the long run.
##' @export
generate_oligo_files <- function(run = "BCS-003",
                                 data = get_lab("pcr_metacommunity"),
                                 primers = "leray",
                                 barcodes = get_lab("tags_primers"),
                                 outdir) {
    run <- match.arg(run, data[["library_id"]])
    d <- data[data[["library_id"]] == run, ]
    d <- d %>%
        dplyr::left_join(barcodes, by = "primer_index_name")

    if (identical("leray", primers))
        primers <- c("GGWACWGGWTGAACWGTWTAYCCYCC",
                     "TANACYTCNGGRTGNCCRAARAAYCA")

    d <- split(d, d$lib_index_name)
    lapply(seq_len(length(d)), function(i) {
        x <- unique(d[[i]]$lib_index_name)
        nm <- substr(x, nchar(x)-1, nchar(x))
        nm <- paste0(i, "-", nm, "_S", i, "_L001.oligos")
        res <- paste("barcode",
                      d[[i]]$primer_index_sequence,
                      d[[i]]$primer_index_sequence,
                      d[[i]]$extraction_id_unique, sep = "\t")
        tf <- file.path(outdir, nm)
        message(res)
        cat("primer", "\t", primers[1], "\t", primers[2], "\n", file = tf)
        cat(res, sep = "\n", file = tf, append = TRUE)
        tf
    })


}
