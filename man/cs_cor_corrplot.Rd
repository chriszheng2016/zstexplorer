% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_cs_cor_corrplot.R
\name{cs_cor_corrplot}
\alias{cs_cor_corrplot}
\alias{cs_cor_corrplot_ui}
\alias{cs_cor_corrplot_server}
\alias{cs_cor_corrplot_app}
\title{cs_cor_corrplot}
\usage{
cs_cor_corrplot_ui(id)

cs_cor_corrplot_server(id, csbl_vars)

cs_cor_corrplot_app(use_online_data = FALSE)
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
A shiny module for cs_cor_corrplot.
}
\details{
The module is an UI for user to display plots of correlation
by \code{\link[corrplot:corrplot]{corrplot}} package.
}
\section{Functions}{
\itemize{
\item \code{cs_cor_corrplot_ui}: UI function of cs_cor_corrplot.

\item \code{cs_cor_corrplot_server}: Server function of cs_cor_corrplot.

\item \code{cs_cor_corrplot_app}: Testing App of cs_cor_corrplot.
}}

\examples{
\dontrun{
# Set up control UI in app UI
ui <- fluidPage(
  cs_cor_corrplot_ui("cs_cor_corrplot_module")
)

# Call control server in App server
server <- function(input, output, session) {
  cs_cor_corrplot <- cs_cor_corrplot_server("cs_cor_corrplot_module")
}

# Run testing App for integration testing
cs_cor_corrplot_app()
}

}
