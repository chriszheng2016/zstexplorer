#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#'
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  # Get factors info
  stock_db <- zstmodelr::stock_db(
    zstmodelr::gta_db,
    get_golem_config("database_dsn")
  )
  zstmodelr::open_stock_db(stock_db)
  factors_info <- zstmodelr::get_factors_info(stock_db, factor_groups = NULL)
  zstmodelr::close_stock_db(stock_db)

  app_module <- golem::get_golem_options("app_module")

  switch(app_module,
    "explore_factor" = {
      explore_factor_server("explore_factor_module",
        factors_info = reactive(factors_info)
      )
    }
  )
}
