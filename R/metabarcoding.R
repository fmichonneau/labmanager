create_demultiplex_data <- function(library_id, file = file.path("/tmp", paste0(library_id, ".csv")),
                                    pcr_meta = get_lab("pcr_metacommunity"),
                                    tag_lib = get_lab("tags_libraries"),
                                    tag_primers = get_lab("tags_primers")) {
    pcr_meta %>%
        dplyr::filter(library_id == library_id) %>%
        dplyr::left_join(tag_lib, by = "lib_index_name") %>%
        dplyr::left_join(tag_primers, by = "primer_index_name") %>%
        dplyr::select(extraction_id,
                      primer_index_name,
                      primer_index_sequence,
                      lib_index_name,
                      lib_index_sequence) %>%
        readr::write_csv(path = file)

}
