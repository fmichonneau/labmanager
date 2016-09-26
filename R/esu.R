
get_esu_by_voucher_number <- function(ids, sample_esu = get_lab("sample_esu")) {
    check_voucher_number(ids, sample_esu)
    res <- sample_esu[match(ids, sample_esu$voucher_number), ]
    paste(res$phylum, res$group_esu, sep = "-")
}
