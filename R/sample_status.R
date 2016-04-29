
#This function outputs a table showing whether or not the sample has had a 
#sequence extraction: 1 if it did, 0 if not
has_seq_extract <-  function(phylum_listings, extraction_table){
  phylum_listings %>%
    left_join(extraction_table) %>%
    group_by(voucher_number) %>%
    summarize(has_extraction = n())
}


#In this function I find the number of PCR sequencing attempts for each voucher.
count_pcr_trials <- function(pcr_data){
  select(pcr_data, voucher_number, pcr_success) %>%
    group_by(voucher_number) %>%
    summarize(number_pcr_attempts = n())
    
}


#This function generates a table showing which vouchers were successful at PCR.
summarize_pcr_successes <- function(pcr_data){
  select(pcr_data, voucher_number, pcr_success) %>%
    filter(pcr_success==1) %>%
    group_by(voucher_number) %>%
    summarise(pcr_successful= n())
  
}


#In this function I create a table named has_seq_file using the list.files()
#function that shows whether each voucher as a sequence file associated with it. 
has_seq_file <- function(seq_path, pcr_table) {
  seq_files <-  list.files(path = seq_path) 
  has_seq_file <-  pcr_table[,"voucher_number"] %in% seq_files
  has_seq_file <- as.numeric(has_seq_file)
  has_seq_file <-  data.frame("voucher_number" = pcr_table[,"voucher_number"],
                              "has_sequence_file" = has_seq_file, stringsAsFactors = FALSE)
  has_seq_file
}

#This function produces a table that lists whether or not each 
#voucher was successful in being sequenced.
seq_success <-  function(sequencing_plate_data, pcr_table){
  seq_success_tbl <- sequencing_plate_data %>%
    left_join(pcr_table) %>%
    select(voucher_number, sequence_success = success)
  seq_success_tbl
}

#This function combines all the tables I create in functions above.
sample_status <- function(pcr_tbl = get_lab("pcr_sample_data"), 
                         ext_tbl = get_lab("extraction_data"),
                         phy_listings = get_lab("sample_data"), 
                         seq_plate_data = get_lab("sequencing_plate_data"),
                         seq_pth = "seqs/COI"){
  seq_ext_tbl <-  has_seq_extract(phy_listings, ext_tbl)
  pcr_attempt_tbl <- count_pcr_trials(pcr_tbl) %>%
    left_join(phy_listings) %>%
    select(voucher_number, phylum, number_pcr_attempts)
  pcr_success_tbl <- summarize_pcr_successes(pcr_tbl)
  seq_file_tbl <- has_seq_file(seq_pth, pcr_tbl)
  seq_success_tbl <-  seq_success(seq_plate_data, pcr_tbl)
  seq_ext_tbl %>%
    left_join(pcr_attempt_tbl) %>%
    left_join(pcr_success_tbl) %>%
    left_join(seq_file_tbl) %>%
    left_join(seq_success_tbl)
}