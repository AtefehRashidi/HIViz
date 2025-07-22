#' launch the HIViz Shiny App
#'
#' This function launches the Shiny application.
#'
#' @examples
#'if(interactive()){
#'HIViz::launchApp()
#'}
#' @export
launchApp <- function() {
  appDir <- system.file("app", package = "HIViz")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}