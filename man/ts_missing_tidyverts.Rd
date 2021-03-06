% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_ts_missing_tidyverts.R
\name{ts_missing_tidyverts}
\alias{ts_missing_tidyverts}
\alias{ts_missing_tidyverts_ui}
\alias{ts_missing_tidyverts_server}
\alias{ts_missing_tidyverts_app}
\title{ts_missing_tidyverts}
\usage{
ts_missing_tidyverts_ui(id)

ts_missing_tidyverts_server(id, tsbl_vars, tsbl_vars_average)

ts_missing_tidyverts_app(use_online_data = FALSE)
}
\arguments{
\item{id}{An ID string of module to connecting UI function and Server
function.}

\item{tsbl_vars}{A tsibble of vars of time series.}

\item{tsbl_vars_average}{A tsibble of average of vars of time series.}

\item{use_online_data}{A logical to determine whether to use test data from
database or not. Default FALSE means to use achieved data for tests.}
}
\value{
\itemize{
\item UI function doesn't return value.
}

\itemize{
\item Server function return return a list of tsibbles of time series
after processing missing values, which contains following fields:
\itemize{
\item tidy_tsbl_vars: a tsibble of variables after processing missing value.
\item tidy_tsbl_vars_average: a tsibble of average of variables after
processing missing value.
}
}
}
\description{
A shiny module for ts_missing_tidyverts.
}
\details{
The module is an UI for user to explore missing values in time series
by tidyverts family packages.
}
\section{Functions}{
\itemize{
\item \code{ts_missing_tidyverts_ui}: UI function of ts_missing_tidyverts.

\item \code{ts_missing_tidyverts_server}: Server function of ts_missing_tidyverts.

\item \code{ts_missing_tidyverts_app}: Testing App of ts_missing_tidyverts.
}}

\examples{
\dontrun{
# Set up control UI in app UI
ui <- fluidPage(
  ts_missing_tidyverts_ui("ts_missing_tidyverts_module")
)

# Call control server in App server
server <- function(input, output, session) {
  ts_missing_tidyverts <- ts_missing_tidyverts_server(
    "ts_missing_tidyverts_module",
    tsbl_vars = reactive(tsbl_vars),
    tsbl_vars_average = reactive(tsbl_vars_average)
  )
}

# Run testing App for integration testing
ts_missing_tidyverts_app()
}

}
