library(shiny)
library(leaflet)
library(labmanager)
library(dplyr)

## Generate the exif information
## info <- system('exiftool -T -r -FileName -Directory -ImageSize "/home/francois/hdd/plankton-images/archive_photos/"',
##                inter=TRUE)
## saveRDS(info, file = "inst/shiny-examples/limsr/photo_info.rds")
## img_df <- read.delim2(textConnection(info),
##                       stringsAsFactors = FALSE,
##                       header = FALSE,
##                       col.names = c("file_name", "file_path", "image_size"))
## library(dplyr)
## img_df <- img_df %>%
##     mutate(image_width = unlist(strsplit(image_size, "x"))[1],
##            image_height = unlist(strsplit(image_size, "x"))[2]) %>%
##     mutate(full_path = file.path(file_path, file_name))
## saveRDS(img_df, file = "inst/shiny-examples/limsr/img_df.rds")
img_df <- readRDS("img_df.rds")


shinyServer(function(input, output) {
    smpl <- get_lab("sample_data")
    seq <- get_lab("sequencing_plate_data")
    sta <- get_lab("station_data")

    img_path <- reactive({
        file.path("~/hdd/plankton-images/archive_photos/", input$voucher_id)
    })

    species_info <- function(species) {
        sp <- strsplit(species, " ")[[1]]
        list(phylum = sp[[1]],
             genus = sp[[3]],
             species = sp[[4]])
    }

    species_voucher <- function(input_species) {
        sp_info <- species_info(input_species)
        ids <- filter(seq,
                      seq[["bold_phylum_id"]] == sp_info$phylum,
                      seq[["bold_genus_id"]] == sp_info$genus,
                      seq[["bold_species_id"]] == sp_info$species) %>%
            select(voucher_number) %>%
            .[[1]]
    }


    output$voucher_selected <- renderText({
        paste("Information about", input$voucher_id)
    })

    output$voucher_phylum <- renderText({
        phylum <- smpl[smpl[["voucher_number"]] == input$voucher_id, "phylum"]
        paste("Phylum:", phylum)
    })

    output$voucher_taxa <- renderText({
        taxa <- smpl[smpl[["voucher_number"]] == input$voucher_id, "field_identification"]
        paste("Taxa:", taxa)
    })

    output$voucher_has_sequence <- renderText({
        seq_info <- seq[seq[["voucher_number"]] == input$voucher_id, ]
        if (nrow(seq_info) == 1 && seq_info[["success"]] == 1L)
            has_seq <- "yes"
        else
            has_seq <- "no"
        paste("Has sequence:", has_seq)
    })

    output$number_images <- renderText({
        paste("Number of images for specimen",
              length(list.files(path = img_path(), pattern = "JPG$"))
              )
    })

    assemble_img <- function(lst_files, voucher) {
        message(voucher)
        lapply(seq_along(lst_files), function(i) {
            output[[paste0(voucher, i)]] <- renderImage({
                return(list(
                    src = lst_files[i],
                    filetype = "image/jpeg",
                    height = 200,
                    width = 300
                ))
            }, deleteFile = FALSE)
        })
    }

    render_img <- function(lst_files, voucher) {
        if (length(lst_files) > 0) {
            res <- lapply(seq_along(assemble_img(lst_files, voucher)), function(i) {
                imageOutput(paste0(voucher, i), height = "255px")
            })
        } else {
            res <- textOutput("No photo...")
        }
    }

    output[["list_img"]] <- renderUI({
        lst_files <- list.files(path = file.path(img_path(), 'thumbs'),
                                pattern = "JPG$", full.names = TRUE)
        render_img(lst_files, basename(img_path()))
    })

    output[["list_img_species"]] <- renderUI({
        vchr <- species_voucher()
        lst_files <- lapply(vchr, function(vchr_) {
            img_pth <- file.path("~/hdd/plankton-images/archive_photos", vchr_)
            img_pth <- img_pth[file.exists(img_pth)]
            if (length(img_pth) > 0)
                list.files(path = file.path(img_pth, 'thumbs'),
                           pattern = "JPG$", full.names = TRUE)
            else character(0)
        })
        lst_files <- unlist(lst_files)
        render_img(lst_files, paste0(vchr, collapse = ""))
    })

    output$voucher_list <- renderText({
       paste("Voucher:", paste0(species_voucher(input$species), collapse = ", "))
    })

    ## Map
    points <- reactive({
        which_smpl <- smpl[smpl[["voucher_number"]] == input$voucher_id, ]
        which_smpl <- dplyr::left_join(which_smpl, sta, by = "station_number")[, c("latitude_start", "longitude_start")]
        names(which_smpl) <- c("latitude", "longitude")
        which_smpl

    })

    output$station_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = points())
    })

    species_points <- function(vchr) {
        filter(smpl, voucher_number %in% vchr) %>%
            left_join(sta, by = "station_number") %>%
            select(latitude = latitude_start,
                   longitude = longitude_start) %>%
            distinct
    }

    output$species_station_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = species_points())
    })
})
