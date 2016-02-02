subset_voucher <- function(vchr_list, path = "~/Documents/plankton-larvae-data") {
    vchr <- get_lab("sample_data", path = path)

    if (!is.null(vchr_list)) {
        assertthat::assert_that(is.character(vchr))
        vchr <- vchr[vchr[["voucher_number"]] %in% vchr_list, ]
    }

    vchr
}

write_bold_csv <- function(res, which_table, path) {
    write.csv(data.frame(res, check.names = FALSE, stringsAsFactors = FALSE),
              row.names = FALSE,
              file = file.path(path, "bold_uploads",
                               paste0(format(Sys.Date(), "%Y%m%d"),
                                      "-", which_table, ".csv"))
              )
}

bold_voucher_table <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {
    vchr <- subset_voucher(vchr_list = vchr_list, path = path)

    res <- list(
        `Sample ID` = character(0),
        `Field ID` = character(0),
        `Museum ID` = character(0),
        `Collection Code` = character(0),
        `Institution Storing` = character(0)
    )

    res[["Sample ID"]] <- res[["Field ID"]] <- vchr[["voucher_number"]]
    res[["Museum ID"]] <- res[["Collection Code"]] <- rep("", nrow(vchr))
    res[["Institution Storing"]] <- rep("Florida Museum of Natural History",
                                        nrow(vchr))

    write_bold_csv(res, "voucher-table", path)
}


bold_taxonomy_table <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {
     vchr <- subset_voucher(vchr_list = vchr_list, path = path)

     res <- list(
        "Phylum" = character(0),
	"Class"	 = character(0),
        "Order" = character(0),
	"Family" = character(0),
	"Subfamily" = character(0),
	"Genus" = character(0),
	"Species" = character(0),
	"Identifier" = character(0),
	"Identifier Email" = character(0),
	"Identifier Institution" = character(0),
	"Identification Method" = character(0),
	"Taxonomy Notes" = character(0)
    )

     res[["Phylum"]] <- vchr[["phylum"]]
     res[["Class"]] <- res[["Order"]] <- res[["Family"]] <- res[["Subfamily"]] <-
         res[["Genus"]] <- res[["Species"]] <- res[["Taxonomy Notes"]] <-
         rep("", nrow(vchr))

     res[["Identifier"]] <- rep("Francois Michonneau", nrow(vchr))
     res[["Identifier Email"]] <- rep("francois.michonneau@gmail.com", nrow(vchr))
     res[["Identifier Institution"]] <- rep("Florida Museum of Natural History", nrow(vchr))
     res[["Identification Method"]] <- rep("Morphology", nrow(vchr))
     write_bold_csv(res, "taxonomy-table", path)
}


bold_specimen_details_table <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {
    vchr <- subset_voucher(vchr_list = vchr_list, path = path)

    res <- list(
        "Sex" = character(0),
        "Reproduction" = character(0),
	"Life Stage" = character(0),
	"Extra Info" = character(0),
        "Notes" = character(0),
	"Voucher Status" = character(0),
	"Tissue Descriptor" = character(0),
	"Associated Taxa" = character(0),
	"Associated Specimens" = character(0),
	"External URLs" = character(0)
    )

    res[["Sex"]] <- res[["Reproduction"]] <-
        res[["Life Stage"]] <-
        res[["Extra Info"]] <-
        res[["Notes"]] <- res[["Associated Taxa"]] <-
        res[["Associated Specimens"]] <-
        res[["External URLs"]] <- rep("", nrow(vchr))

    res[["Voucher Status"]] <- ifelse(vchr[["has_voucher"]] == 1,
                                      "to be vouchered:holdup/private",
                               ifelse(vchr[["has_photo"]] == 1,
                                      "e-vouchered:dna/tissue+photo",
                                      "dna/tissue vouchered only"))
    res[["Tissue Descriptor"]] <- ifelse(vchr[["is_plankton"]] == 1,
                                         "larva", "body part")
    write_bold_csv(res, "specimen-details", path)
}

bold_collection_data_table <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {

    vchr <- subset_voucher(vchr_list = vchr_list, path = path)

    sta <- get_lab("station_data", path = path)

    vchr <- dplyr::left_join(vchr, sta, by = "station_number")

    res <- list(
        "Collectors" = character(0),
	"Collection Date" = character(0),
	"Country/Ocean" = character(0),
	"State/Province" = character(0),
	"Region" = character(0),
	"Sector" = character(0),
	"Exact Site" = character(0),
	"Latitude" = character(0),
	"Longitude" = character(0),
	"Elevation" = character(0),
	"Depth" = character(0),
	"Elevation Precision" = character(0),
	"Depth Precision" =  character(0),
        "GPS Source" = character(0),
	"Coordinate Accuracy" = character(0),
	"Event Time" = character(0),
	"Collection Date Accuracy" = character(0),
	"Habitat" = character(0),
	"Sampling Protocol" = character(0),
	"Collection Notes" = character(0),
	"Site Code" = character(0),
	"Collection Event ID" = character(0)
    )

    res[["Collectors"]] <- rep("Francois Michonneau", nrow(vchr))
    res[["Collection Date"]] <- paste(vchr[["year"]],
                                      sprintf("%02d", as.numeric(vchr[["month"]])),
                                      sprintf("%02d", as.numeric(vchr[["day"]])),
                                      sep = "-")
    res[["Country/Ocean"]] <- rep("Ocean Atlantic", nrow(vchr))
    res[["State/Province"]] <- rep("Florida", nrow(vchr))
    res[["Region"]] <- rep("", nrow(vchr)) #rep("St Johns County", nrow(vchr))
    res[["Sector"]] <- res[["Elevation"]] <- res[["Elevation Precision"]] <-
        res[["GPS Source"]] <- res[["Event Time"]] <-
        res[["Collection Notes"]] <- res[["Site Code"]] <-
        rep("", nrow(vchr))
    res[["Exact Site"]] <- rep("", nrow(vchr)) #rep("Matanzas River", nrow(vchr))
    res[["Latitude"]] <- vchr[["latitude_start"]]
    res[["Longitude"]] <- vchr[["longitude_start"]]
    res[["Depth"]] <- rep("1", nrow(vchr))
    res[["Depth Precision"]] <- rep("1", nrow(vchr))
    res[["Coordinate Accuracy"]] <- rep("0.1km", nrow(vchr))
    res[["Collection Date Accuracy"]] <- rep("0", nrow(vchr))
    res[["Habitat"]] <- rep("coastal inlet(ENVO:00000137)", nrow(vchr))
    res[["Sampling Protocol"]] <- vchr[["sampling_device"]]
    res[["Collection Event ID"]] <- vchr[["station_number"]]

    res <- lapply(res, function(x) {
        x[which(is.na(x))] <- ""
        x
    })

    write_bold_csv(res, "collection-data", path)
}


generate_bold_tables <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {
    bold_voucher_table(vchr_list = vchr_list, path = path)
    bold_taxonomy_table(vchr_list = vchr_list, path = path)
    bold_specimen_details_table(vchr_list = vchr_list, path = path)
    bold_collection_data_table(vchr_list = vchr_list, path = path)
}

generate_bold_fasta <- function(vchr_list = NULL, path = "~/Documents/plankton-larvae-data") {
    vchr <- subset_voucher(vchr_list = vchr_list, path = path)
    seqs <- get_lab("sequencing_plate_data", path = path)
    res <- seqs[seqs[["voucher_number"]] %in% vchr[["voucher_number"]] &
                seqs[["success"]] == 1, ] %>%
        dplyr::select(voucher_number)
    chopper::mergeSeq(res[[1]], output = file.path(path, "bold_uploads",
                                              paste0(format(Sys.Date(), "%Y%m%d"),
                                                     "-sequences")),
                      seqFolder = file.path(path, "seqs"), markers = "COI",
                      convertEnds = FALSE)
}
