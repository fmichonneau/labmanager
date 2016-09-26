export_data_by_phylum <- function(phylum, path = "/tmp") {
    get_photos_phylum(phylum)
    get_sequences_by_phylum(phylum, fasta_file = file.path("/tmp", phylum, paste0(tolower(phylum), ".fasta")))
}
