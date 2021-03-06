% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_cs_missing_visdat.R
\name{cs_missing_visdat}
\alias{cs_missing_visdat}
\alias{cs_missing_visdat_ui}
\alias{cs_missing_visdat_server}
\alias{cs_missing_visdat_app}
\title{cs_missing_visdat}
\usage{
cs_missing_visdat_ui(id)

cs_missing_visdat_server(id, csbl_vars)

cs_missing_visdat_app(use_online_data = FALSE)
}
\arguments{
\item{id}{An ID string of module to connecting UI function and Server
function.}

\item{csbl_vars}{A tibble of vars of cross-section.}

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
A shiny module for cs_missing_visdat.
}
\details{
The module is an UI for user to display missing pattern by
\code{\link[visdat:visdat]{visdat}} package.
}
\section{Functions}{
\itemize{
\item \code{cs_missing_visdat_ui}: UI function of cs_missing_visdat.

\item \code{cs_missing_visdat_server}: Server function of cs_missing_visdat.

\item \code{cs_missing_visdat_app}: Testing App of cs_missing_visdat.
}}

\examples{
\dontrun{
# Set up control UI in app UI
ui <- fluidPage(
  cs_missing_visdat_ui("cs_missing_visdat_module")
)

# Call control server in App server
server <- function(input, output, session) {
  cs_missing_visdat <- cs_missing_visdat_server(
     "cs_missing_visdat_module"
     csbl_vars = reactive(csbl_vars)
  )
}

# Run testing App for integration testing
cs_missing_visdat_app()
}

}
