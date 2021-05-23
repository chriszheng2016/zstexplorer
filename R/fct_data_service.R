
# Global variable for setting
global_data_setting <- list(
  stock_db = NULL
)

#' Get singleton of stock database
#'
#' Get the singleton of stock database of the App for database operation.
#'
#' @return A object of stock_db class.
#'
#' @examples
#' \dontrun{
#'
#' # Method A
#' stock_db <- stock_db()
#' factors_info <- zstmodelr::get_stock_info(stock_db)
#'
#' # Method B
#' factors_info <- zstmodelr::get_stock_info(stock_db())
#' }
#'
#' @noRd
stock_db <- function() {

  # Open single instance of stock database
  if (is.null(global_data_setting$stock_db)) {
    stock_db <- zstmodelr::stock_db(
      zstmodelr::gta_db,
      get_golem_config("database_dsn")
    )
    zstmodelr::open_stock_db(stock_db)
    zstmodelr::init_stock_db(stock_db)
    # global_data_setting$stock_db <<- stock_db
    assign("global_data_setting$stock_db", value = stock_db, inherits = TRUE)
  } else {
    stock_db <- global_data_setting$stock_db
  }

  invisible(stock_db)
}

#' Translate codes to names
#'
#' It support translation between code and names for stock, industry, factor,
#' industry, and mixed codes among them.
#'
#' @param codes A character or vector of codes to match.
#'  a code could be a regular expression for matching in non-exact way.
#'
#' @param exact_match	 A logic to determine use exact matching or not.
#' Default FALSE means to non-exact matching
#'
#' @return  A character or vector of matched names.
#'
#' @examples
#' \dontrun{
#'
#' # Exact matching
#' code2name(c("600031", "600030"))
#'
#' # Non-exact matching
#' code2name(c("60003", "60004"))
#'
#' # Regular expression matching
#' code2name(c("031$"))
#'
#' # Mixed codes to names
#' code2name(c("600031", "C28", "GPM", "f050101b"))
#'
#' }
#'
#' @noRd
code2name <- function(codes, exact_match = FALSE) {

  # Function to translate a code to name
  .single_code2name <- function(code, stock_db, exact_match) {
    type_list <- c("stock", "industry", "factor", "indicator")
    success <- FALSE
    index <- 1
    while ((!success) && (index <= length(type_list))) {
      # Match code to name
      name <- zstmodelr::code2name(
        stock_db,
        code = code,
        exact_match = exact_match,
        type = type_list[index]
      )
      success <- !all(is.na(name))
      index <- index + 1
    }

    name
  }

  # Main function
  suppressMessages({
    stock_db <- stock_db()
  })

  # Translate multiple codes to names
  names <- purrr::map(
    codes,
    .f = ~ .single_code2name(.x, stock_db = stock_db, exact_match = exact_match)
  ) %>% purrr::flatten_chr()

  names
}


#' Translate names to codes
#'
#' It support translation between code and names for stock, industry, factor,
#' industry.
#'
#' @param names A character or vector of names to match.
#' a name could be a regular expression for matching in non-exact way.
#'
#' @param exact_match	 A logic to determine use exact matching or not.
#' Default FALSE means to use non-exact matching
#'
#' @return  A character or vector of matched names.
#' @examples
#' \dontrun{
#'
#' # Exact matching
#' name2code(c("Gross profit margin", "Operating profit margin"))
#'
#' # Non-exact matching
#' name2code(c("Gross", "Operating"))
#'
#' # Regular expression matching
#' name2code("margin$")
#'
#' # Mixed names to codes
#' names <- code2name(c("600031", "C28", "GPM", "f050101b"))
#' codes <- name2code(names)
#'
#' }
#' @noRd
name2code <- function(names, exact_match = FALSE) {

  # Function to translate a name to code
  .single_name2code <- function(name, stock_db, exact_match) {
    type_list <- c("stock", "industry", "factor", "indicator")
    success <- FALSE
    index <- 1
    while ((!success) && (index <= length(type_list))) {
      # Match name to code
      code <- zstmodelr::name2code(
        stock_db,
        name = name,
        exact_match = exact_match, type = type_list[index]
      )
      success <- !all(is.na(code))
      index <- index + 1
    }

    code
  }

  # Main function
  suppressMessages({
    stock_db <- stock_db()
  })

  # Translate multiple names to codes
  codes <- purrr::map(
    names,
    .f = ~ .single_name2code(.x, stock_db = stock_db, exact_match = exact_match)
  )%>%
  purrr::flatten_chr()

  return(codes)
}
