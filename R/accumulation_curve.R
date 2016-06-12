##' @importFrom dplyr left_join select mutate group_by %>% summarize
n_esu_per_collecting_event <- function(phylum = "") {
    smpl <- get_lab("sample_data")
    esus <- get_lab("sample_esu")
    stat <- get_lab("station_data")

    if (nzchar(phylum)) {
        smpl <- smpl[smpl$"phylum" == phylum, ]
    }

    summ_tbl <- dplyr:::left_join(smpl, stat, by = "station_number") %>%
        dplyr::select(voucher_number, station_number, phylum,
                      field_identification, year, month, day) %>%
        dplyr::left_join(esus, by = "voucher_number") %>%
        dplyr::filter(!is.na(group_esu)) %>%
        dplyr::mutate(phylum_esu = paste(.$"phylum.y",
                                         .$"group_esu",
                                         sep = "-")) %>%
        dplyr::mutate(date = as.Date(paste(.$"year",
                                           .$"month",
                                           .$"day", sep = "-"),
                                     format = "%Y-%m-%d"))


    res2 <- summ_tbl %>%
        group_by(phylum_esu) %>%
        summarize(first_seen = min(date, na.rm = T)) %>%
        group_by(first_seen) %>%
        summarize(n_esu_per_date = n())

    res3 <- summ_tbl %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(n_esu = n_distinct(phylum_esu),
                         n_smpl = n_distinct(voucher_number))

    res <- left_join(res2, res3, by = c("first_seen" = "date")) %>%
        mutate(cum_sum = cumsum(n_esu_per_date),
               prop_new_esu = n_esu_per_date/n_esu,
               prop_new_smp = n_esu_per_date/n_smpl)

    res
}



if (FALSE) {
    library(dplyr)
    library(ggplot2)


    total <- n_esu_per_collecting_event() %>%
        mutate(phylum = rep("TOTAL", nrow(.)))

    arth <- n_esu_per_collecting_event("Arthropoda") %>%
        mutate(phylum = rep("Arthropoda", nrow(.)))

    annel <- n_esu_per_collecting_event("Annelida") %>%
        mutate(phylum = rep("Annelida", nrow(.)))

    mollu <- n_esu_per_collecting_event("Mollusca") %>%
        mutate(phylum = rep("Mollusca", nrow(.)))

    res <- bind_rows(total, annel, mollu, arth)


    svg(file = "/tmp/accumulation.svg", height = 5)
    ggplot(res,
           aes(x = first_seen, y = cum_sum, colour = phylum)) + geom_line() +
        xlab("Date") + ylab("Cumulative number of species sampled") +
        scale_x_date(date_labels = "%Y-%m-%d") +
        theme(axis.text.y = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 12))
    dev.off()

    ggplot(n_esu_per_collecting_event(phylum = "Annelida"),
           aes(x = first_seen, y = cum_sum)) + geom_line() +
        xlab("Date") + ylab("Cumulative number of species sampled") +
        scale_x_date(date_labels = "%Y-%m-%d")


    ggplot(res) + geom_line(aes(x = first_seen, y = prop_new_esu))
    ggplot(res) + geom_line(aes(x = first_seen, y = prop_new_smp))

}
