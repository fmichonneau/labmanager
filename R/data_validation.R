
#This function checks if the voucher numbers in the PCR data, extraction data
# and the sequencing plate data are in the phylum listings.
voucher_num_present <- function(pcr_tbl= get_lab("pcr_sample_data"),
                                phylum_tbl= get_lab("sample_data"),
                                ext_tbl = get_lab("extraction_data"),
                                seq_plate_tbl= get_lab("sequencing_plate_data")){
  in_pcr <-  pcr_tbl$voucher_number %in% phylum_tbl$voucher_number
  in_ext <-  ext_tbl$voucher_number %in% phylum_tbl$voucher_number
  in_seq_plate <-  seq_plate_tbl$voucher_number %in% phylum_tbl$voucher_number
  if (!all(in_pcr)){
    not_in_pcr <- pcr_tbl$voucher_number[!in_pcr]
    warning("Some vouchers in PCR data not in phylum listings:", paste(not_in_pcr, collapse= ", "))
  }
  if (!all(in_ext)) {
    not_in_ext <-  ext_tbl$voucher_number[!in_ext]
    warning("Some vouchers in extractions not in phylum listings:", paste(not_in_ext, collapse= ", "))
  }
  if (!all(in_seq_plate)){
    not_in_seq_plate <-  in_seq_plate$voucher_number[!in_seq_plate]
    warning("Some vouchers in sequencing plate data not in phylum listings:",
           paste(not_in_seq_plate, collapse= ", "))
  }
}


#This function checks that there are no duplicate voucher numbers in the phylum listings.
no_duplicates <- function(phylum_tbl = "sample_data"){
  duplicates <-  phylum_tbl$voucher_number[duplicated(phylum_tbl$voucher_number)]
  if (length(duplicates) > 0){
    stop("There are duplicate voucher numbers in the phylum listings", duplicates)
  }
}



#This function attempts to check that each combination of pcr_id and
#voucher_number in the sequencing plate data is also found in the
#PCR data.
comb_present <- function(seq_plate_tbl = get_lab("sequencing_plate_data"),
                        pcr_tbl= get_lab("pcr_sample_data")){
  combs_seq_plate <-  paste(seq_plate_tbl$pcr_id, seq_plate_tbl$voucher_number)
  combs_pcr_data <-  paste(pcr_tbl$pcr_id, pcr_tbl$voucher_number)
  combs_not_present <-  setdiff(combs_seq_plate, combs_pcr_data)
  if (length(combs_not_present) == 0){
    stop("Every combination present.")
  } else{stop("Not every combination present:", combs_not_present)}
}
