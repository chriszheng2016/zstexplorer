
#' @keywords internal
"_PACKAGE"

# Import frequently-used functions from other packages
#' @importFrom stats mad median sd cor na.omit
#' @importFrom utils head
#' @importFrom graphics par
#' @importFrom rlang .data

## Quiets notes of R CMD check for pipelines codes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("where", ".", ":=")
  )
}


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
