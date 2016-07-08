get_voucher_info <- function(voucher, extraction_data = get_lab("extraction_data"),
                             pcr_sample_data = get_lab("pcr_sample_data"),
                             sample_data = get_lab("sample_data"),
                             seq_path){
  
  data_row = sample_data[sample_data[,"voucher_number"]== voucher,]
  
  
  station_number <- data_row[,"station_number"]
  phylum <- data_row[,"phylum"]
  field_identification <- data_row[,"field_identification"]
  is_plankton <- as.logical(data_row[,"is_plankton"])
  has_photo <- as.logical(data_row[,"has_photo"])
  has_voucher <- as.logical(data_row[,"has_voucher"])
  
  data_row_ext = extraction_data[extraction_data[,"voucher_number"] == voucher,]
  
  extraction_date <- data_row_ext[,"extraction_date"]
  second_attempt_date <- data_row_ext[,"second_attempt_date"]
  
  #Here I set the second_attempt_date to NA if it doesn't exist.
  if (second_attempt_date == ""){
    second_attempt_date <- NA
  }
  
  #Here I find the PCR IDs from pcr_sample_data.
  data_rows_pcr = pcr_sample_data[pcr_sample_data[,"voucher_number"]== voucher,]
  pcr_id = data_rows_pcr[,"pcr_id"]
  
  #Here I set the pcr_id to NA if none exist and set the has_sequence value.
  has_sequence <- TRUE
  if (length(pcr_id)== 0){
    pcr_id <- NA
    has_sequence <- FALSE
  }
  
  list(station_number = station_number, 
       phylum = phylum, 
       field_identification = field_identification,
       is_plankton = is_plankton,
       has_photo = has_photo,
       has_voucher = has_voucher,
       extraction_date = extraction_date,
       second_attempt_date = second_attempt_date,
       pcr_id = pcr_id,
       has_sequence = has_sequence)
}
  