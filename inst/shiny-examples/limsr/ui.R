library(shiny)
library(leaflet)
library(labmanager)

voucher <- get_lab("sample_data")
with_img <-  voucher[["voucher_number"]][as.logical(voucher[["has_photo"]])]
##n_img <- sapply(with_img, function(x) length(list.files(path = file.path("~/hdd/plankton-images/archive_photos", x))))

shinyUI(fluidPage(
    titlePanel("Florida plankton"),

    sidebarLayout(
        sidebarPanel(
            "Specimen information",
            selectizeInput(
                'voucher_id', "Voucher ID",
                choices = with_img
            ),
            leafletOutput("station_map")
         ),
        mainPanel(
            h2(textOutput("voucher_selected")),
            textOutput("number_images"),
            textOutput("voucher_phylum"),
            textOutput("voucher_taxa"),
            textOutput("voucher_has_sequence"),
            uiOutput("list_img")
        )
    )
))
