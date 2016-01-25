##' @importFrom storr storr_rds
data_store <- function(path = "~/Documents/plankton-larvae-data/data_storr") {
    st <- storr::storr_rds(path = path)
    st
}
