extraction_table <- read.csv(file = "CopyOfdata/extraction_data.csv", 
                      stringsAsFactors = FALSE)
pcr_table <- read.csv(file = "CopyOfdata/pcr_data.csv", 
                      stringsAsFactors = FALSE)
phylum_listings <- read.csv(file = "CopyOfdata/phylum_listings.csv", 
                      stringsAsFactors = FALSE)
sequencing_plate_data <- read.csv(file = "CopyOfdata/sequencing_plate_data.csv",
                                  stringsAsFactors = FALSE)

get_voucher_info <- function(voucher, extraction_data = extraction_table,
                             pcr_sample_data = pcr_table,
                             sample_data = phylum_listings,
                             seq_path = seq_path){
  
  #Here I find the row number of the voucher in sample_data.
  voucher_num_sample_data <- 0
  for (l in 1:nrow(sample_data)) {
    if (sample_data[l,1] == voucher) {
      voucher_num_sample_data <- l
    }
  }
  
  station_number <- sample_data[voucher_num_sample_data, 2]
  phylum <- sample_data[voucher_num_sample_data, 3]
  field_identification <- sample_data[voucher_num_sample_data, 4]
  is_plankton <- as.logical(sample_data[voucher_num_sample_data, 6])
  has_photo <- as.logical(sample_data[voucher_num_sample_data, 7])
  has_voucher <- as.logical(sample_data[voucher_num_sample_data, 8])
  
  #Here I find the row number of the voucher in extraction_data.
  voucher_num_ext_data <- 0
  for (e in 1:nrow(extraction_data)){
    if (extraction_data[e,1] == voucher){
      voucher_num_ext_data <- e
    }
  }
  
  extraction_date <- extraction_data[voucher_num_ext_data, 3]
  second_attempt_date <- extraction_data[voucher_num_ext_data, 5]
  
  #Here I set the second_attempt_date to NA if it doesn't exist.
  if (second_attempt_date == ""){
    second_attempt_date <- NA
  }
  
  #Here I find the PCR IDs from pcr_sample_data.
  pcr_id = c()
  for (p in 1:nrow(pcr_sample_data)){
    if (pcr_sample_data[p,2] == voucher){
      pcr_id = c(pcr_id, pcr_sample_data[p,1])
    }
  }
  
  #Here I set the pcr_id to NA if none exist and set the has_sequence value.
  has_sequence <- TRUE
  if (length(pcr_id)== 0){
    pcr_id <- NA
    has_sequence <- FALSE
  }
  
  list(station_number <- station_number, 
       phylum <- phylum, 
       field_identification <- field_identification,
       is_plankton <- is_plankton,
       has_photo <- has_photo,
       has_voucher <- has_voucher,
       extraction_date <- extraction_date,
       second_attempt_date <- second_attempt_date,
       pcr_id <- pcr_id,
       has_sequence <- has_sequence)
}
  