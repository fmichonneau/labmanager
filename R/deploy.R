
### Not part of the package at this stage.

copy_data <- function(from = "~/Documents/plankton-larvae-data",
                      to = "~/R-dev/flpk-shiny/www/data") {
    if (!dir.exists(to)) {
        message(sQuote(to), " doesn't exist... Creating it.")
        dir.create(to)
    }
    src_data <- list.files(path = from, pattern = "csv$", ignore.case = TRUE,
                           full.names = TRUE)
    to_data <- file.path(to, basename(src_data))
    res <- cbind(src_data, to_data)
    apply(res, 1, function(x) file.copy(x[1], x[2], overwrite = TRUE,
                                        copy.date = TRUE))
}

copy_storr <- function(from = "~/Documents/plankton-larvae-data/data_storr",
                       to = "~/R-dev/flpk-shiny/www/data_storr") {
    if (!dir.exists(to)) {
        message(sQuote(to), " doesn't exist... Creating it.")
        dir.create(to)
    }
    to <- normalizePath(to)
    from <- normalizePath(from)
    lst_files <- list.files(path = from, full.names = TRUE, recursive = TRUE)
    to_data <- gsub(from, to, lst_files)
    res <- cbind(lst_files, to_data)

    sapply(to_data, function(x) {
        x <- dirname(x)
        if (!dir.exists(x)) {
            dir.create(x, recursive = TRUE)
        }
    })

    apply(res, 1, function(x) file.copy(x[1], x[2], overwrite = TRUE,
                                        copy.date = TRUE))
}

copy_photos <- function(from = "~/hdd/plankton-images/app_photos",
                        to = "~/R-dev/flpk-shiny/www/app_photos",
                        testing = FALSE) {

    ## use rsync to copy the photos to not do it everytime
    if (!dir.exists(to)) {
        message(sQuote(to), " doesn't exist... Creating it.")
        dir.create(to)
    }
    to <- normalizePath(to)
    from <- normalizePath(from)
    src_data <- list.files(path = from, pattern = "jpg$", ignore.case = TRUE,
                           recursive = TRUE, full.names = TRUE)
    to_data <- gsub(from, to, src_data)
    res <- cbind(src_data, to_data)

    if (testing) {
        message("testing... so only copying the first 50 images")
        res <- res[1:50, ]
    }

    sapply(to_data, function(x) {
        x <- dirname(x)
        if (!dir.exists(x)) {
            dir.create(x, recursive = TRUE)
        }
    })

    apply(res, 1, function(x) file.copy(x[1], x[2], overwrite = TRUE,
                                        copy.date = TRUE))
}

## @param d a droplet
## @param testing if \code{TRUE}, only a subset of the images are
##     copied to the app to speed up process
deploy_app <- function(d, app_path = "~/R-dev/flpk-shiny",
                       testing = FALSE, ...) {

    ## before deploying we need to copy the photo and the data to www
    copy_data()
    copy_storr()
    copy_photos(testing = testing)
    ## use rsync to only copy
    #analogsea::docklet_shinyapp(droplet = d, path = app_path,
    #                            img = "fmichonneau/flpk-docker-image",
    #                            ...)

}
