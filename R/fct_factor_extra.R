
# Helper function for PCA -------------------------------------------------


#' Get loading/rotation of PCA results
#'
#' @param pca_res An object of class PCA ([`FactoMineR`][FactoMineR::FactoMineR]);
#' prcomp and princomp(([`stats`][stats::stats]); pca, dudi('adea4`);
#' epPCA([`ExPosition`]::[ExPosition::ExPosition]).
#'
#' @param pc_name_prefix A character as name prefix of primary components in
#' results, e.g PC for PC1...PCn. Default NULL means use original name of
#' primary components.
#'
#' @param ncp	 A number of primary components kept in the results. Default Inf means all
#' primary components.
#'
#' @return A matrix of loading or rotation of PCA results.
#' @export
#'
#' @examples
#' \donttest{
#'
#' # Result from prcomp
#' pca_res_prcomp <- prcomp(iris[, -5], scale = TRUE)
#' pca_loading <- get_pca_loading(pca_res_prcomp)
#'
#' # Result from princomp
#' pca_res_princomp <- princomp(scale(iris[, -5]))
#' pca_loading <- get_pca_loading(pca_res_princomp)
#'
#' # Result from FactoMineR::PCA
#' pca_res_PCA <- FactoMineR::PCA(iris[, -5], ncp = ncol(iris[, -5]), graph = FALSE)
#' pca_loading <- get_pca_loading(pca_res_PCA)
#' }
#'
get_pca_loading <- function(pca_res,
                            pc_name_prefix = NULL,
                            ncp = Inf) {
  # Validate parameters
  if (!is.null(pc_name_prefix)) {
    assertive::assert_is_character(pc_name_prefix)
  }
  assertive::assert_is_numeric(ncp)
  assertive::assert_all_are_greater_than_or_equal_to(ncp, 0)

  pca_var <- factoextra::get_pca_var(pca_res)
  var_coord <- pca_var$coord

  pca_eig <- factoextra::get_eig(pca_res)
  comp_sdev <- sqrt(pca_eig[1:ncol(var_coord), "eigenvalue"])

  # Function to compute loading value
  loading_value <- function(var_coord, comp_sdev) {
    var_coord / comp_sdev
  }

  # Compute loading matrix
  loading_matrix <- t(apply(var_coord, MARGIN = 1, FUN = loading_value, comp_sdev))

  # Fix names of PCs if need
  if (!is.null(pc_name_prefix)) {
    pc_names <- colnames(loading_matrix)
    pc_names <- stringr::str_replace(pc_names,
      pattern = "[^\\d]+",
      replacement = pc_name_prefix
    )
    colnames(loading_matrix) <- pc_names
  }

  # Subset loading matrix if need
  if (!is.infinite(ncp)) {
    ncp <- min(NCOL(loading_matrix), ncp)
    loading_matrix <- loading_matrix[, 1:ncp, drop = FALSE]
  }

  loading_matrix
}


#' Get equation formula for loading of PCA result
#'
#' Get equation formula for loading of PCA results in MathJax formats which can
#' be display in browser.
#'
#' @inheritParams get_pca_loading
#'
#' @param digits How many significant digits are to be used for coefficient in
#' formula.
#'
#' @return A vector of Character of loading formula of Primary Components
#' @export
#'
#' @examples
#' \donttest{
#'
#' # Display formula in uiOutput(ns("pca_result_formula"))
#' # Server function
#' output$pca_result_formula <- renderUI({
#'   pca_res <- prcomp(iris[, -5], scale = TRUE)
#'
#'   # Get formula of primary components of PCA
#'   pca_formula <- get_pca_formula(pca_res)
#'
#'   pca_formula_mathJax <- purrr::map(pca_formula, .f = helpText)
#'
#'   withMathJax(
#'     pca_formula_mathJax
#'   )
#' })
#' }
#'
get_pca_formula <- function(pca_res,
                            pc_name_prefix = NULL,
                            ncp = Inf,
                            digits = 3) {

  # Validate parameters
  if (!is.null(pc_name_prefix)) {
    assertive::assert_is_character(pc_name_prefix)
  }
  assertive::assert_is_numeric(ncp)
  assertive::assert_all_are_greater_than_or_equal_to(ncp, 0)
  assertive::assert_is_numeric(digits)
  assertive::assert_all_are_greater_than_or_equal_to(digits, 0)

  # Function to build a formula for one PC
  pc_formula_func <- function(loading_values, var_names = NULL, pc_name = "pc1") {
    if (is.null(var_names)) {
      var_names <- names(loading_values)
    }

    formula_string <- glue::glue("$${pc_name}=")
    for (i in seq_along(loading_values)) {
      var_name <- var_names[i]
      if ((i == 1) && (loading_values[i] >= 0)) {

        # coef <- sprintf("%.3f", loading_values[i])
        coef <- sprintf(glue::glue("%.{digits}f"), loading_values[i])
      } else {
        # coef <- sprintf("%+.3f", loading_values[i])
        coef <- sprintf(glue::glue("%+.{digits}f"), loading_values[i])
      }

      formula_string <- glue::glue(
        "{formula_string}{coef}{var_name}"
      )
    }

    formula_string <- glue::glue("{formula_string}$$")

    formula_string
  }

  # Main function
  pca_loading <- get_pca_loading(pca_res, pc_name_prefix, ncp)
  df_pca_loading <- tibble::as_tibble(t(pca_loading), rownames = "PC")
  if (!is.null(pc_name_prefix)) {
    pc_names <- colnames(df_pca_loading)
    df_pca_loading <- df_pca_loading %>%
      dplyr::mutate(
        PC = stringr::str_replace(.data$PC,
          pattern = "[^\\d]+",
          replacement = pc_name_prefix
        )
      )
  }

  pca_formula <- df_pca_loading %>%
    dplyr::nest_by(.data$PC) %>%
    dplyr::summarise(
      formula = pc_formula_func(.data$data, pc_name = .data$PC),
      .groups = "keep"
    ) %>%
    dplyr::pull(.data$formula)

  pca_formula
}
