##' @importFrom storr storr_rds
data_store <- function(path = "~/Documents/plankton-larvae-data/data_storr") {
    st <- storr::storr_rds(path = path)
    st
}

alg_store <- function(path = "~/Documents/plankton-larvae-data/alg_storr") {
    st <- storr::storr_external(storr::driver_rds(path = path),
                                fetch_hook_alignment)
    st
}

seq_store <- storr::storr_rds(path = "~/Documents/plankton-larvae-data/seq_storr")
