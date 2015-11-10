
## library(dplyr)

## pcr_samples <- read.csv(file = "~/Documents/plankton-larvae-data/pcr_sample_data.csv",
##                         stringsAsFactors = FALSE)
## extractions <- read.csv(file = "~/Documents/plankton-larvae-data/extraction_data.csv",
##                         stringsAsFactors = FALSE)
## samples <- read.csv(file = "~/Documents/plankton-larvae-data/sample_data.csv",
##                     stringsAsFactors = FALSE)

## samples %>%
##     merge(pcr_samples, by = "voucher_number") %>%
##     filter(pcr_success == 0)


##

validate_sequencing_plate <- function(plate_id) {

    seq_plate <- get_lab("sequencing_plate_data")
    pcr_sample <- get_lab("pcr_sample_data")

    ## 96 samples
    n_samples <- sum(nzchar(seq_plate[["voucher_number"]]))
    if (n_samples != 96)
        message("There should be 96 samples, there are: ", n_samples,
                "There are probably some blank cells.")

    ## no duplicated pcr samples
    no_dup <- seq_plate[seq_plate[["sequencing_plate_id"]] ==  plate_id,
                        "voucher_number"]

    if (length(no_dup) != 96) message("There should be 96 samples, there are: ",
                                      length(no_dup))

    if (any(duplicated(no_dup))) message("Some samples are duplicated: ",
                                         paste(no_dup[duplicated(no_dup)],
                                               collapse = ", "))

    ## all pcr samples worked
    tmp_seq_plate <- seq_plate
    tmp_seq_plate[["pcr_key"]] <- paste(tmp_seq_plate[["pcr_id"]],
                                        tmp_seq_plate[["voucher_number"]],
                                        sep = "-")
    tmp_pcr_sample <- pcr_sample
    tmp_pcr_sample[["pcr_key"]] <- paste(pcr_sample[["pcr_id"]],
                                         pcr_sample[["voucher_number"]],
                                         sep = "-")
    check_success <- dplyr::left_join(tmp_seq_plate, tmp_pcr_sample, by = "pcr_key")

    if (any(check_success[["pcr_success"]] != 1L))
        message("Some samples included in plate ",
                sQuote(plate_id), " were not successful: ",
                paste(check_success[check_success[["pcr_success"]] != 1L, "pcr_key"],
                      collapse = ", "))
}

build_plate_map <- function(plate_id) {

    seq_plate <- get_lab("sequencing_plate_data")
    pcr <- get_lab("pcr_sample_data")

    sample_plate <- seq_plate[seq_plate[["sequencing_plate_id"]] ==  plate_id,
                       c("pcr_id", "voucher_number", "plate_column", "plate_row")]

    primer_plate <- dplyr::left_join(sample_plate[, c("pcr_id", "plate_column", "plate_row")],
                                     pcr[, c("pcr_id", "pcr_primer_forward", "pcr_primer_reverse")],
                                     by = "pcr_id")

    plate_map <- tidyr::spread_(sample_plate[, -match("pcr_id", names(sample_plate))],
                                "plate_column", "voucher_number")

    ## It works for now because the first place is all the same locus, but will need to be
    ## smarter if plate with mixed loci...
    ##primer_map <- tidyr::spread_(sample_plate[, -match()])

    colnames(plate_map)[1] <- ""
    plate_map
}

##' @importFrom magrittr %>%
##' @importFrom dplyr group_by_ summarize_ left_join select_ mutate_ rename_ tally
failed_pcr <- function(pcr_samples = get_lab("pcr_sample_data"),
                       extracts = get_lab("extraction_data"),
                       samples = get_lab("sample_data")) {
    failed_pcr <- pcr_samples[pcr_samples[["pcr_success"]] == 0L, ]

    n_attempts <- failed_pcr %>%
        dplyr::group_by_("voucher_number") %>%
        dplyr::tally(.) %>%
        dplyr::rename_("number_pcr_attempts" = "n")

    tmp_res <- dplyr::left_join(n_attempts,
                                dplyr::select_(
                                    samples,
                                    "voucher_number",
                                    "phylum",
                                    "field_identification"
                                ),
                                by = "voucher_number") %>%
        dplyr::left_join(extracts[, c("voucher_number", "second_attempt_protocol_id")],
                         by = "voucher_number") %>%
        dplyr::mutate_("second_attempt_protocol_id" = is.na("second_attempt_protocol_id")) %>%
        dplyr::rename_("second_attempt" = "second_attempt_protocol_id") %>%
        dplyr::select_("voucher_number", "phylum",
                       "field_identification", "number_pcr_attempts",
                       "second_attempt")
    complete <- tmp_res[["number_pcr_attempts"]] >= 2 &
        tmp_res[["second_attempt"]]
    message(sum(complete), " have been extracted twice, and been PCR'ed twice without success.")
    as.data.frame(tmp_res[!complete, ])
}

successful_pcr_not_yet_plated <- function(pcr_samples = get_lab("pcr_sample_data"),
                                          sequencing_plates = get_lab("sequencing_plate_data")) {
    pcr_success <- pcr_samples[pcr_samples[["pcr_success"]] == 1L &
                               pcr_samples[["ignore_for_plating"]] != 1L, ]
    uniq_pcr_success <- paste(pcr_success[["pcr_id"]],
                              pcr_success[["voucher_number"]],
                              sep = "_")

    uniq_pcr_plated <- paste(sequencing_plates[["pcr_id"]],
                             sequencing_plates[["voucher_number"]],
                             sep = "_")

    not_plated <- setdiff(uniq_pcr_success, uniq_pcr_plated)

    pcr_voucher <- gsub("(.+)_(.+)", "\\2", not_plated)

    ## Will need to be smarter when dealing with multiple loci
    if (any(duplicated(pcr_voucher))) {
        warning("Some samples seem to be duplicated: ",
                paste(pcr_voucher[duplicated(pcr_voucher)], collapse = ", "))
    }

    not_plated <- strsplit(not_plated, "_")
    not_plated <- do.call("rbind", not_plated)
    not_plated <- data.frame(pcr_id = not_plated[, 1],
                             voucher_number = not_plated[, 2],
                             stringsAsFactors = FALSE)
    not_plated

}


render_plate_map <- function(plate_id, outfile = "/tmp/test.pdf") {

    plate_map_template <- "
---
title: 'Plate: {{plate_id}}'
output: pdf_document
classoption: landscape
geometry: margin=0.2in
fontsize: 8pt
---

Loaded on: {{loaded_on}}

Submitted on: {{submitted_on}}


{{{table}}}

"
    seq_plate <- get_lab("sequence_plate_data")
    seq_id <- get_lab("sequence_plate_id")

    map_data <- list(
        plate_id = plate_id,
        loaded_on = seq_id[seq_id[["sequencing_plate_id"]] == plate_id, "loaded_on"],
        submitted_on = seq_id[seq_id[["sequencing_plate_id"]] == plate_id, "submitted_on"],
        table =  paste(knitr::kable(build_plate_map(plate_id), collapse = "\n"))
    )

    ftmp <- tempfile()

    cat(whisker::whisker.render(plate_map_template,
                                map_data),
        file = ftmp, sep = "\n")
    tt <- readLines(ftmp)
    tt <- gsub(",", "\n", tt)
    cat(tt,  file = ftmp, sep = "\n")

    rmarkdown::render(ftmp, output_format = "pdf_document", output_file = outfile)

}


generate_icbr_template <- function(plate_id, primer,
                                   outfile = "/tmp/icbr_form.csv",
                                   sequencing_plate = get_lab("sequencing_plate_data")) {
    seq_data <- sequencing_plate[sequencing_plate[["sequencing_plate_id"]] == plate_id, ]
    well <- paste0(seq_data[["plate_row"]],
                   sprintf("%02d", seq_data[["plate_column"]]))
    write.csv(cbind(WELL = well,
                    TEMPLATE = seq_data[["voucher_number"]],
                    PRIMER = primer),
              file = outfile, row.names = FALSE)
}

success_summary <- function(pcr_id, samples = get_lab("sample_data"),
                            pcr_samples = get_lab("pcr_sample_data")) {
    tmp_pcr <- pcr_samples[pcr_samples[["pcr_id"]] %in% pcr_id,  ]
    tmp_pcr <- dplyr::left_join(tmp_pcr, samples, by = "voucher_number")
    xtabs(~ pcr_success + phylum, tmp_pcr)
}
