
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

copy_storr <- function(from, to) {
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
                        to = "~/R-dev/flpk-shiny/www/app_photos") {

    if (!dir.exists(to)) {
        message(sQuote(to), " doesn't exist... Creating it.")
        dir.create(to)
    }
    from <- normalizePath(from)
    to <- normalizePath(to)

    rsync_cmd <- paste(
        "rsync -r",
        from,
        to
    )
    system(rsync_cmd)
}

## @param d a droplet
## @param testing if \code{TRUE}, only a subset of the images are
##     copied to the app to speed up process
deploy_app <- function(d, app_path = "~/R-dev/flpk-shiny",
                       ...) {

    ## before deploying we need to copy the photo and the data to www
    copy_data()
    copy_storr(from = "~/Documents/plankton-larvae-data/data_storr",
               to = "~/R-dev/flpk-shiny/www/data_storr")
    copy_storr(from = "~/Documents/plankton-larvae-data/bold_data_store",
               to = "~/R-dev/flpk-shiny/www/bold_data_store")
    copy_photos()
    ## use rsync to only copy
    #analogsea::docklet_shinyapp(droplet = d, path = app_path,
    #                            img = "fmichonneau/flpk-docker-image",
    #                            ...)

}
