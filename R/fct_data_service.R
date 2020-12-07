
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
#'  # Method A
#'  stock_db <- stock_db()
#'  factors_info <- zstmodelr::get_stock_info(stock_db)
#'
#'  # Method B
#'  factors_info <- zstmodelr::get_stock_info(stock_db())
#'
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
    global_data_setting$stock_db <<- stock_db
  } else {
    stock_db <- global_data_setting$stock_db
  }

  invisible(stock_db)
}

#' Translate codes to names
#'
#' It support translation between code and names for stock, industry, factor,
#' industry.
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
#'  # Exact matching
#'  code2name(c("600031", "600030"))
#'
#'  # Non-exact matching
#'  code2name(c("60003", "60004"))
#'
#'  # Regular expression matching
#'  code2name(c("031$"))
#'
#' }
#'
#' @noRd
code2name <- function(codes, exact_match = FALSE) {
  suppressMessages({
    # Fetch factors information from database
    stock_db <- stock_db()


    type_list <- c("stock", "industry", "factor", "indicator")
    success <- FALSE
    index <- 1
    while ((!success) && (index <= length(type_list))) {
      # Match code to name
      names <- zstmodelr::code2name(stock_db, code = codes,
                                    exact_match = exact_match, type = type_list[index])
      success <- !all(is.na(names))
      index <- index + 1
    }
  })

  return(names)
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
#'  # Exact matching
#'  name2code(c("Gross profit margin", "Operating profit margin"))
#'
#'  # Non-exact matching
#'  name2code(c("Gross", "Operating"))
#'
#'  # Regular expression matching
#'  name2code("margin$")
#'
#' }
#' @noRd
name2code <- function(names, exact_match = FALSE) {
  suppressMessages({
    # Fetch factors information from database
    stock_db <- stock_db()


    type_list <- c("stock", "industry", "factor", "indicator")
    success <- FALSE
    index <- 1
    while ((!success) && (index <= length(type_list))) {
      # Match name to code
      codes <- zstmodelr::name2code(stock_db, name = names,
                                    exact_match = exact_match, type = type_list[index])
      success <- !all(is.na(codes))
      index <- index + 1
    }
  })

  return(codes)
}
