validate <- function(what, res) {
    if (identical(what, "sequencing_plate_data")) {
        ## no spaces in taxa names
        tt <- sapply(c("bold_phylum_id", "bold_order_id", "bold_family_id", "bold_genus_id", "bold_species_id"),
                     function(x) {
            with_spaces <- sapply(res[[x]], nchar)
            no_spaces <- sapply(res[[x]], function(i) nchar(gsub("\\s", "", i)))
            if (any(with_spaces != no_spaces)) stop("there are spaces in the taxa names of ", what)
        })
    }
}
