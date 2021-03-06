% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_ts_analysis.R
\name{ts_analysis}
\alias{ts_analysis}
\alias{ts_analysis_ui}
\alias{ts_analysis_server}
\alias{ts_analysis_app}
\title{ts_analysis}
\usage{
ts_analysis_ui(id)

ts_analysis_server(id, tsbl_vars, debug = FALSE)

ts_analysis_app(use_online_data = FALSE, debug = on_debug())
}
\arguments{
\item{id}{An ID string of module to connecting UI function and Server
function.}

\item{tsbl_vars}{A tsibble of vars for time-series analysis.}

\item{debug}{A logic to enable debug or not, default is on_debug() which
returns DEBUG environment variable.}

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
A shiny module for ts_analysis.
}
\details{
The module is an UI for user to analyze variables in time series.
}
\section{Functions}{
\itemize{
\item \code{ts_analysis_ui}: UI function of ts_analysis.

\item \code{ts_analysis_server}: Server function of ts_analysis.

\item \code{ts_analysis_app}: Testing App of ts_analysis.
}}

\examples{
\dontrun{
# Set up control UI in app UI
ui <- fluidPage(
  ts_analysis_ui("ts_analysis_module")
)

# Call control server in App server
server <- function(input, output, session) {
  ts_analysis <- ts_analysis_server("ts_analysis_module",
    tsbl_vars = reactive(tsbl_vars)
  )
}

# Run testing App for integration testing
ts_analysis_app()
}

}
