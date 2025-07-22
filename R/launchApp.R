#' Launch the HIViz Shiny App
#'
#' This function launches the Shiny application.
#'
#' @return Invisible \code{NULL}. Called for its side effects (launches the
#'   Shiny application in the default browser).
#'
#' @examples
#' if (interactive()) {
#'   HIViz::launchApp()
#' }
#' @export
launchApp <- function() {
  appDir <- system.file("app", package = "HIViz")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}