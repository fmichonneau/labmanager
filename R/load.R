load_data <- function(...) {
    read.csv(...,  stringsAsFactors = FALSE)
}

lab_data_files <- function(path, full.names = FALSE) {
    files <- list.files(path = path, pattern = "csv$", full.names = TRUE)
    if (full.names) return(files)
    gsub("\\.csv$", "", basename(files))
}

load_lab_data <- function(path, ...) {
    files <- lab_data_files(path, full.names = TRUE)
    res <- lapply(files, function(f) load_data(file = f, ...))
    names(res) <- gsub("\\.csv$", "", basename(files))
    res
}

##' Load the data associated with the project
##'
##' @param what the name of the CSV file to load
##' @param path where the CSV files for the project live
##' @param ... additional parameters to be passed to read.csv (stringsAsFactors is set to FALSE)
##' @export
get_lab <- function(what, path = "~/Documents/plankton-larvae-data", ...) {
    if (missing(what))
        stop(sQuote("what"), " needs to be specified")

    what <- match.arg(what, lab_data_files(path))
    res <- load_lab_data(path, ...)[[what]]
    validate_lab(what, res)
    res
}
