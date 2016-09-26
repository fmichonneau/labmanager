
get_voucher_numbers_by_phylum <- function(phylum, sample_data = get_lab("sample_data")) {
    check_phylum(phylum)
    res <- sample_data[sample_data$phylum %in% phylum, ]
    split(res$voucher_number, res$phylum)
}
