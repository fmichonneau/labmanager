seq_plate_data = read.csv(file = "~/labmanager/R/CopyOfdata/sequencing_plate_data.csv",
                       stringsAsFactors= FALSE)

filter_by_taxa = function(taxonomic_level,taxonomic_value) {
  if (taxonomic_level == "Phylum" | taxonomic_level == "phylum"){
    seq_plate_row = seq_plate_data[seq_plate_data$bold_phylum_id == taxonomic_value,]
  } else if (taxonomic_level == "Class" | taxonomic_level == "class"){
    seq_plate_row = seq_plate_data[seq_plate_data$bold_class_id == taxonomic_value,]
  } else if (taxonomic_level == "Order" | taxonomic_level == "order") {
    seq_plate_row = seq_plate_data[seq_plate_data$bold_order_id == taxonomic_value,]
  } else if (taxonomic_level == "Family" | taxonomic_level == "family") {
    seq_plate_row = seq_plate_data[seq_plate_data$bold_family_id == taxonomic_value,]
  } else if (taxonomic_level == "Genus" | taxonomic_level == "genus") {
    seq_plate_row = seq_plate_data[seq_plate_data$bold_genus_id == taxonomic_value,]
  } else if (taxonomic_level == "Species" | taxonomic_level == "species") {
    seq_plate_row = seq_plate_data[seq_plate_data$bold_species_id == taxonomic_value,]
  }
  voucher_nums = seq_plate_row$voucher_number
  voucher_nums = na.omit(voucher_nums)
  paste(voucher_nums, sep = " ")
}