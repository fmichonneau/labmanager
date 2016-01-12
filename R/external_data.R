if (FALSE) {

    library(dplyr)

    no_echino <- read.csv(file = "~/Documents/plankton-larvae-data/external_data/uf_barcodes_no_echinoderms.csv",
                          stringsAsFactors = FALSE) %>%
        select(plate = Plate,
               cell = Cell,
               sample = UF.field.ID,
               collection_code = Collection.Code,
               catalog_number = Catalog.number,
               phylum = Phylum,
               class = class_,
               order = order_,
               family,
               genus,
               species,
               dna_id = DNA.ID,
               genus_or_higher = genus.or.higher,
               locality = Loc,
               notes = Notes,
               sequence = Sequence) %>%
        select( -dna_id, -notes)

    echino <- read.csv(file = "~/Documents/plankton-larvae-data/external_data/uf_barcodes_echinoderms.csv",
                       stringsAsFactors = FALSE) %>%
        filter(CAS == "UF") %>%
        select(plate = Plate.Extraction.Genbank,
               cell = Cell,
               sample = Sample,
               collection_code = CAS,
               catalog_number = Catalog_number,
               class,
               order,
               family,
               genus = x_genus,
               genus_or_higher = genusorhigher,
               species,
               locality = Loc,
               pass.seq,
               sequence = Sequence) %>%
        filter(pass.seq == "yes") %>%
        select(-pass.seq) %>%
        mutate(phylum = rep("Echinodermata", nrow(.)))

    combined <- rbind(echino, no_echino)

    count_bases <- function(x) {
        nchar(gsub("[^actg]", "", x, ignore.case = TRUE))
    }

    seq_length <- count_bases(combined$sequence)

    combined <- combined[seq_length >= 550, ]

    generate_fasta <- function(db, label = c("phylum",
                                             "genus_or_higher",
                                             "species", "locality",
                                             "collection_code",
                                             "sample"),
                               outfile) {
        labs <- sapply(seq_len(nrow(db)),
                       function(x) paste(db[x, label], collapse = "_"))
        labs <- gsub("UF_", "UF", labs)
        labs <- gsub("\\s", "_", labs)
        labs <- gsub("_{2,}", "_", labs)

        res <- mapply(function(lab, seq) {
            paste0(">", lab, "\n", seq, "\n")
        },
        labs[!duplicated(labs)], db[["sequence"]][!duplicated(labs)])

        cat(res, sep = "", file = outfile)
        outfile
    }

    library(chopper)

}
