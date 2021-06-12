# Auxiliary functions about testing data of the project----

#' Load test data of factors_info in tibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @return A tsibble of factors information.
#' @export
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
#' @export
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
        names_from = "factor_code",
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
#' @export
load_csbl_vars <- function(use_online_data = FALSE) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  # Convert vars time-series vars to vars of cross-section
  csbl_vars <- tsbl2csbl(tsbl_vars)

  return(csbl_vars)
}

#' Load stock return time-series in tsibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#' @param period_type A character of period type, like "month", "quarter",
#'  "year".
#' @param stock_codes A character vector of stock codes to load. NULL means
#'  to load all stocks.
#'
#' @return A tsibble of stock return time-series.
#' @export
load_stock_return <- function(use_online_data = FALSE,
                              period_type = c("month", "quarter", "year"),
                              stock_codes = c("000651", "000333", "600066",
                                "000550", "600031", "000157")
                              ){
  # Prepare data
  if (use_online_data) {

    # Load data from database
    stock_db <- zstmodelr::stock_db(
      zstmodelr::gta_db,
      get_golem_config("database_dsn")
    )
    zstmodelr::open_stock_db(stock_db)
    zstmodelr::init_stock_db(stock_db)

    stock_info <- zstmodelr::get_stock_info(stock_db)

    # Fetch selected stock return from database
    period_type <- match.arg(period_type)
    if (period_type == "quarter") {
      # Aggregate month data into quarter data since there is not quarter
      # data in database
      tbl_return <- zstmodelr::get_stock_return(
        stock_db,
        stock_cd_list = stock_codes,
        period_type = "month",
        period_date = "end",
        output_type = "tibble"
      ) %>%
        zstmodelr::ts_resample(
          freq_rule = "quarter",
          agg_fun = ~ (prod(1 + .x, na.rm = TRUE) - 1),
          date_index_fields = "date",
          key_fields = "stkcd"
        )
    } else {
      # Directly fetch data from database
      tbl_return <- zstmodelr::get_stock_return(
        stock_db,
        stock_cd_list = stock_codes,
        period_type = period_type,
        period_date = "end",
        output_type = "tibble"
      )
    }

    # Add period fields
    tbl_return <- tbl_return %>%
      dplyr::mutate(period = period_type)

    # Add indcd fields
    if (!("indcd" %in% names(tbl_return))) {
      if ("stkcd" %in% names(tbl_return)) {
        tbl_return <- tbl_return %>%
          dplyr::left_join(stock_info, by = "stkcd") %>%
          dplyr::select(c(names(tbl_return), "indcd"))
      }
    }

    # Drop any row with NA in key_fields column
    tbl_return <- tbl_return %>%
      tidyr::drop_na(.data[["stkcd"]])

    # Turn into tsibble
    tsbl_return <- tbl_return %>%
      tsibble::as_tsibble(
        index = "date",
        key = c("stkcd")
      ) %>%
      dplyr::select(
        c("date", "stkcd", "indcd", "period"),
        dplyr::everything()
      )

    zstmodelr::close_stock_db(stock_db)
  } else {

    # Load test dataset
    period_type <- match.arg(period_type)
    tsbl_return_file <- fs::path(
      here::here(),
      glue::glue("tests/testthat/data/tsbl_stock_{period_type}_return.rds")
    )
    tsbl_return <- readRDS(tsbl_return_file)
    assertive::assert_is_inherited_from(tsbl_return, c("tbl_ts"))

    if(!is.null(stock_codes)) {
      tsbl_return <- tsbl_return %>%
        dplyr::filter(.data$stkcd %in% stock_codes)
    }
  }

  return(tsbl_return)
}

#' Load market return time-series in tsibble
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @param period_type A character of period type, like "month", "quarter",
#'  "year".
#'
#' @return A tsibble of market return time-series.
#' @export
load_market_return <- function(use_online_data = FALSE,
                              period_type = c("month", "quarter", "year")) {

  # Prepare data
  if (use_online_data) {

    # Load data from database
    stock_db <- zstmodelr::stock_db(
      zstmodelr::gta_db,
      get_golem_config("database_dsn")
    )
    zstmodelr::open_stock_db(stock_db)
    zstmodelr::init_stock_db(stock_db)

    period_type <- match.arg(period_type)
    if (period_type == "quarter") {
      # Aggregate month data into quarter data since there is not quarter
      # data in database
      tbl_return <- zstmodelr::get_market_return(
        stock_db,
        period_type = "month",
        period_date = "end",
        output_type = "tibble"
      ) %>%
        zstmodelr::ts_resample(
          freq_rule = "quarter",
          agg_fun = ~ (prod(1 + .x, na.rm = TRUE) - 1),
          date_index_field = "date"
        )
    } else {
      # Directly fetch data from database
      tbl_return <- zstmodelr::get_market_return(
        stock_db,
        period_type = period_type,
        period_date = "end",
        output_type = "tibble"
      )
    }
    tbl_return <- tbl_return %>%
      dplyr::rename(return = .data$market_index)

    # Add period fields
    tbl_return <- tbl_return %>%
      dplyr::mutate(period = period_type)

    # Turn into tsibble
    tsbl_return <- tbl_return %>%
      tsibble::as_tsibble(
        index = "date"
      )%>%
      dplyr::select(
        c("date", "period"),
        dplyr::everything()
      )

    zstmodelr::close_stock_db(stock_db)
  } else {
    # Load test dataset
    period_type <- match.arg(period_type)
    tsbl_return_file <- fs::path(
      here::here(),
      glue::glue("tests/testthat/data/tsbl_market_{period_type}_return.rds")
    )
    tsbl_return <- readRDS(tsbl_return_file)
    assertive::assert_is_inherited_from(tsbl_return, c("tbl_ts"))
  }

  return(tsbl_return)
}

#' Convert tsbl of vars to csbl of vars
#'
#' Convert vars of time-series into vars of cross-section.
#'
#' @param tsbl_vars A tsibble of time series vars.
#'
#' @return A tibble of vars of cross-section.
#' @export
tsbl2csbl <- function(tsbl_vars) {

  # Validate parameters
  assertive::assert_is_inherited_from(tsbl_vars, c("tbl_ts"))

  # Convert tsbl_vars to csbl_vars
  index_var <- tsibble::index_var(tsbl_vars)
  key_vars <- tsibble::key_vars(tsbl_vars)

  csbl_vars <- tsbl_vars %>%
    tsibble::as_tibble() %>%
    tidyr::unite(col = "id", {{ index_var }}, {{ key_vars }}, remove = TRUE)

  return(csbl_vars)
}

#' Aggregate tsbl_vars by new key field
#'
#' Use new key to aggregate tsbl_vars.
#'
#' `industry_median()`/`industry_mean()` is a wrapper of `aggregate_tsbl_vars()`.
#'
#' @param tsbl_vars A tsibble of time series vars.
#' @param by A character of key field.
#' @param .fun A function to aggregate numeric variables.
#' @param ...  Arguments passed on to .fun.
#'
#' @return A tsibble aggregated by new key
#' @export
aggregate_tsbl_vars <- function(tsbl_vars,
                                by,
                                .fun = median,
                                ...) {

  # Validate parameters
  assertive::assert_is_inherited_from(tsbl_vars, c("tbl_ts"))
  assertive::assert_is_character(by)
  assertive::assert_all_are_true(by %in% names(tsbl_vars))
  assertive::assert_is_function(.fun)

  agg_fun <- purrr::partial(.fun, ...)

  if ("period" %in% names(tsbl_vars)) {
    # Need Special treatment for period field
    agg_tsbl_vars <- tsbl_vars %>%
      dplyr::group_by(.data[[by]], .data$period) %>%
      dplyr::summarise(
        period = zstmodelr::mode_value(.data$period),
        dplyr::across(where(is.numeric), ~ agg_fun(.))
      ) %>%
      dplyr::select(date, {{ by }}, dplyr::everything())
  } else {
    agg_tsbl_vars <- tsbl_vars %>%
      dplyr::group_by(.data[[by]]) %>%
      dplyr::summarise(
        dplyr::across(where(is.numeric), ~ agg_fun(.))
      ) %>%
      dplyr::select(date, {{ by }}, dplyr::everything())
  }

  agg_tsbl_vars <- agg_tsbl_vars %>%
    dplyr::ungroup()

  agg_tsbl_vars
}

# Compute mean of tsbl_vars by industries
#' @describeIn aggregate_tsbl_vars Aggregate mean of tsbl_vars by industries.
#' @export
industry_median <- function(tsbl_vars) {
  aggregate_tsbl_vars(tsbl_vars, by = "indcd", .fun = median, na.rm = TRUE)
}

# Compute median of tsbl_vars by industries
#' @describeIn aggregate_tsbl_vars Aggregate median of tsbl_vars by industries.
#' @export
industry_mean <- function(tsbl_vars) {
  aggregate_tsbl_vars(tsbl_vars, by = "indcd", .fun = mean, na.rm = TRUE)
}

#' Transform index of tsbl_var into period date
#'
#' @param tsbl_vars A tsibble of time series vars.
#' @param period_field A character name of field which contain "quarter",
#'  "month", "week" as period indicators. Default is "period".
#'
#' @return A tsibble with with index of periodic date.
#' @export
#'
#' @examples
#' \dontrun{
#' tsbl_vars_new <- periodize_index(tsbl_vars, period_field = "period")
#' }
#'
periodize_index <- function(tsbl_vars, period_field = "period") {

  # Validate parameters
  assertive::assert_is_inherited_from(tsbl_vars, c("tbl_ts"))
  assertive::assert_all_are_true(period_field %in% names(tsbl_vars))


  index_var <- tsibble::index_var(tsbl_vars)
  key_vars <- tsibble::key_vars(tsbl_vars)

  # Make tsibble index as period date
  period <- unique(tsbl_vars[[period_field]])
  assertive::assert_all_are_equal_to(length(period), 1)
  period_date_fun <- switch(period,
    "quarter" = tsibble::yearquarter,
    "month" = tsibble::yearmonth,
    "week" = tsibble::yearweek,
    as.Date
  )

  tsbl_vars_new <- tsbl_vars %>%
    dplyr::mutate(!!index_var := period_date_fun(.data[[index_var]])) %>%
    dplyr::select(-c({{ period_field }}))

  tsbl_vars_new
}
