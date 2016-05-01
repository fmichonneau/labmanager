
if (FALSE) {

    library(dplyr)
    library(ggplot2)
    library(wesanderson)
    library(tidyr)

    ## histogram number of species per phylum
    dat <- get_lab("sample_esu") %>%
        dplyr::filter(phylum != "Metazoa") %>%
        dplyr::group_by(phylum) %>%
        dplyr::summarize(
            n_esu = n_distinct(group_esu)
        )

    dat$phylum <- reorder(dat$phylum, dat$n_esu, sort)

    svg(file = "~/Documents/2016-01.NFMSS/diversity_histo.svg", height = 5)
    ggplot(dat, aes(x = phylum, y = n_esu)) +
        geom_bar(stat = "identity", fill = wes_palette("Zissou")[1]) +
        coord_flip() +
        theme(axis.text = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 12)) +
        xlab("") + ylab("Number of species")
    dev.off()

    ## histogram number of species that have a match per phylum
    gg <- get_lab("sequencing_plate_data") %>%
        filter(success == 1) %>%
        filter(!is.na(bold_phylum_id)) %>%
        group_by(bold_phylum_id) %>%
        summarize(
            n_spp = n_distinct(bold_species_id)
        ) %>%
        ungroup %>%
        rename(phylum = bold_phylum_id) %>%
        right_join(dat)

    gg$phylum <- reorder(gg$phylum, gg$n_esu, sort)

    res <- gg %>%
        gather(N, n_species, -phylum)

    res$N <- factor(res$N, levels = c("n_spp", "n_esu"))

    pal <- wes_palette("Zissou", 5)

    svg(file = "~/Documents/2016-01.NFMSS/percent_identified.svg", height = 5)

    ggplot(res, aes(x = phylum, y = n_species, fill = N)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        theme(axis.text = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              legend.position = "none") +
        xlab("") + ylab("Number of species")


    dev.off()


}
