% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchApp.R
\name{launchApp}
\alias{launchApp}
\title{Launch the HIViz Shiny App}
\usage{
launchApp()
}
\value{
Invisible \code{NULL}. Called for its side effects (launches the
Shiny application in the default browser).
}
\description{
This function launches the Shiny application.
}
\examples{
if (interactive()) {
  HIViz::launchApp()
}
}
