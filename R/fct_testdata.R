# Auxiliary functions about testing data of the project----

#' Load test data of factors_info in tibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @return A tsibble of factors information.
#'
load_factors_info <- function(use_online_data = FALSE) {

  # Prepare data
  if (use_online_data) {
    # Fetch factors information from database
    stock_db <- zstmodelr::stock_db(
      zstmodelr::gta_db,
      get_golem_config("database_dsn")
    )
    zstmodelr::open_stock_db(stock_db)
    zstmodelr::init_stock_db(stock_db)
    factors_info <- zstmodelr::get_factors_info(stock_db, factor_groups = NULL)
    zstmodelr::close_stock_db(stock_db)
  } else {
    # Load test dataset
    factors_info_file <- fs::path(
      here::here(), "tests/testthat/data/factors_info.rds"
    )
    factors_info <- readRDS(factors_info_file)
    assertive::assert_is_inherited_from(factors_info, c("tbl_df"))
  }

  return(factors_info)
}


#' Load test data of vars of time-series in tsibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @return A tsibble of vars of time-series.
#'

load_tsbl_vars <- function(use_online_data = FALSE) {

  # Prepare data
  if (use_online_data) {
    # Load data from database
    stock_db <- zstmodelr::stock_db(
      zstmodelr::gta_db,
      get_golem_config("database_dsn")
    )
    zstmodelr::open_stock_db(stock_db)
    zstmodelr::init_stock_db(stock_db)

    # Fetch selected factors from database
    select_factors <- c("CFOR", "CR", "QR", "TDR", "ICR", "CFCR")
    ds_factors <-
      zstmodelr::get_factors(stock_db, factor_codes = select_factors) %>%
      tidyr::pivot_wider(
        names_from = "factor_name",
        values_from = "factor_value"
      )

    # Turn into tsibble
    tsbl_vars <- tsibble::as_tsibble(ds_factors,
      index = date,
      key = c("period", "stkcd")
    )
    zstmodelr::close_stock_db(stock_db)
  } else {
    # Load test dataset
    tsbl_vars_file <- fs::path(here::here(), "tests/testthat/data/tsbl_vars.rds")
    tsbl_vars <- readRDS(tsbl_vars_file)
    assertive::assert_is_inherited_from(tsbl_vars, c("tbl_ts"))
  }

  return(tsbl_vars)
}


#' Load test data of vars of cross-section in tibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @return A tibble of vars of cross-section.


load_csbl_vars <- function(use_online_data = FALSE) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  # Convert vars time-series vars to vars of cross-section
  csbl_vars <- tsbl2csbl(tsbl_vars)

  return(csbl_vars)
}



#' Convert tsbl of vars to csbl of vars
#'
#' Convert vars of time-series into vars of cross-section.
#'
#' @param tsbl_vars A tsibble of time series vars.
#'
#' @return A tibble of vars of cross-section.
#'
tsbl2csbl <- function(tsbl_vars) {

  # Validate parameters
  assertive::assert_is_inherited_from(tsbl_vars, c("tbl_ts"))

  # Convert tsbl_vars to csbl_vars
  index_var <- tsibble::index_var(tsbl_vars)
  key_vars <- tsibble::key_vars(tsbl_vars)

  csbl_vars <- tsbl_vars %>%
    tsibble::as_tibble() %>%
    tidyr::unite(col = "id", {{index_var}},{{key_vars}}, remove = TRUE)

  return(csbl_vars)
}
