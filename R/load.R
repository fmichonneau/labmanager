load_data <- function(...) {
    read.csv(...,  stringsAsFactors = FALSE)
}

lab_data_files <- function(path, full.names = FALSE) {
    files <- list.files(path = path, pattern = "csv$", full.names = TRUE)
    if (full.names) return(files)
    gsub("\\.csv$", "", basename(files))
}

load_saved_hashes <- function(path) {
    hash_file <- file.path(path, "hashes.rds")
    if (file.exists(hash_file)) {
        readRDS(hash_file)
    } else {
        NULL
    }
}

load_lab_data <- function(path = "~/Documents/plankton-larvae-data", ...) {
    ## need to be cleaned up
    files <- lab_data_files(path, full.names = TRUE)
    shrt_files <- basename(files)

    lab_data_file <- file.path(path, "lab_data.rds")
    hashes <- hash_files(files)
    names(hashes) <- basename(names(hashes))

    prev_hashes <- load_saved_hashes(path)

    ##if (is.null(prev_hashes) || !file.exists(lab_data_file)) {
        res <- lapply(files, function(f) load_data(file = f, ...))
        names(res) <- gsub("\\.csv$", "", basename(files))
        saveRDS(hashes, file = file.path(path, "hashes.rds"))
    ## } else {
    ##     res <- readRDS(file = lab_data_file)
    ##     if ((!identical(length(res), length(files))) ||
    ##         (all(names(res) %in% files) &&
    ##          all(files %in% names(res)))) {
    ##         res <- lapply(files, function(f) load_data(file = f, ...))
    ##         names(res) <- gsub("\\.csv$", "", basename(files))
    ##         saveRDS(hashes, file = file.path(path, "hashes.rds"))
    ##     } else {
    ##         for (i in seq_along(shrt_files)) {
    ##             if (!identical(prev_hashes[shrt_files[i]], hashes[shrt_files[i]])) {
    ##                 res[[files[i]]] <- load_data(file = files[i])
    ##             }
    ##         }
    ##         saveRDS(hashes, file = file.path(path, "hashes.rds"))
    ##     }
    ## }
    ## saveRDS(res, file = lab_data_file)
    res
}

get_lab <- function(what, path = "~/Documents/plankton-larvae-data") {
    if (missing(what))
        stop(sQuote("what"), " needs to be specified")
    else {
        what <- match.arg(what, lab_data_files(path))
        res <- load_lab_data()[[what]]
        validate(what, res)
        res
    }
}


hash_files <- function(filenames, named=TRUE) {
  if (is.null(filenames)) {
    filenames <- character(0)
  }
  hash <- tools::md5sum(filenames)
  if (named) hash else unname(hash)
}
