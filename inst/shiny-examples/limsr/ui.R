library(shiny)
library(leaflet)
library(labmanager)

voucher <- get_lab("sample_data")
with_img <-  voucher[["voucher_number"]][as.logical(voucher[["has_photo"]])]

sequencing_data <- get_lab("sequencing_plate_data") %>%
    filter(!is.na(bold_phylum_id)) %>%
    filter(bold_phylum_id != "")
lst_bold_species <- paste(sequencing_data[["bold_phylum_id"]], "--",
                          sequencing_data[["bold_genus_id"]],
                          sequencing_data[["bold_species_id"]],
                          paste0("(", sequencing_data[["bold_bin_id"]], ")")) %>%
    sort

shinyUI(
    navbarPage("Florida plankton",
               tabPanel("by voucher",
                        sidebarLayout(
                            sidebarPanel("Specimen information",
                                         selectizeInput('voucher_id', "Voucher ID",
                                                        choices = with_img),
                                         leafletOutput("station_map")
                                         ),
                            mainPanel(
                                h2(textOutput("voucher_selected")),
                                textOutput("number_images"),
                                textOutput("voucher_phylum"),
                                textOutput("voucher_taxa"),
                                textOutput("voucher_has_sequence"),
                                uiOutput("list_img")),
                            )
                        ),
               tabPanel("By species",
                        sidebarLayout(
                            sidebarPanel("Choose the species",
                                         selectizeInput('species', "Species",
                                                        choices = lst_bold_species)
                                         ),
                            uiOutput("list_img_species")
                        )
                        )
               )
)
