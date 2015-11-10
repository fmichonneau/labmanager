##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Run limsr
##' @return Start the shiny app
##' @author Francois Michonneau
##' @export
##' @importFrom shiny runApp
run_limsr <- function() {
    app_dir <- system.file("shiny-examples", "limsr", package = "labmanager")
    if (app_dir == "") {
        stop("Couldn't find limsr, please install ", sQuote("labmanager"))
    }
    shiny::runApp(app_dir, display.mode = "normal")
}
