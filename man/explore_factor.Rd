% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_explore_factor.R
\name{explore_factor}
\alias{explore_factor}
\alias{explore_factor_ui}
\alias{explore_factor_server}
\alias{explore_factor_app}
\title{Explore factors in tsibble}
\usage{
explore_factor_ui(id)

explore_factor_server(id)

explore_factor_app(use_online_data = FALSE)
}
\arguments{
\item{id}{An ID string of module to connecting UI function and Server
function.}

\item{use_online_data}{A logical to determine whether to use test data from
database or not. Default FALSE means to use achieved data for tests.}
}
\value{
\itemize{
\item UI function doesn't return value.
}

\itemize{
\item Server function doesn't return value.
}
}
\description{
A shiny module to explore factors in tibble of time series(tsibble).
}
\details{
The module is an UI for user to explore factors.
}
\section{Functions}{
\itemize{
\item \code{explore_factor_ui}: UI function of exploring factors.

\item \code{explore_factor_server}: Server function of exploring factors.

\item \code{explore_factor_app}: Testing App of exploring factors.
}}

\examples{
\dontrun{
# Set up control UI in app UI
ui <- fluidPage(
  explore_factor_ui("explore_factor_module")
)

# Call control server in App server
server <- function(input, output, session) {
  explore_factor_server("explore_factor_module")
}

# Run testing App for integration testing
explore_factor__app()
}

}
