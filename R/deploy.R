
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

copy_photos <- function(from = "~/hdd/plankton-images/app_photos",
                        to = "~/R-dev/flpk-shiny/www/app_photos",
                        testing = FALSE) {
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

    apply(res, 1, function(x) file.copy(x[1], x[2], overwrite = TRUE,
                                        copy.date = TRUE))
}

## @param d a droplet
## @param testing if \code{TRUE}, only a subset of the images are
##     copied to the app to speed up process
deploy_app <- function(d, app_path = "~/R-dev/flpk-shiny",
                       testing = FALSE) {

    ## before deploying we need to copy the photo and the data to www
    copy_data()
    copy_photos()
    analogsea::docklet_shinyapp(droplet = d, path = app_path,
                                img = "fmichonneau/flpk-docker-image",
                                ...)

}
