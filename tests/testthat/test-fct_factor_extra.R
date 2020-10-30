# Tests for extra functions for factor ----

test_that("get_pca_loading, with various arguments", {

  # get_pca_loading on default arguments  ====
  pca_res <- prcomp(iris[, -5],  scale = TRUE)
  pca_loading <- get_pca_loading(pca_res)
  expect_colnames <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4")
  expect_rownames <- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
  #expect_s3_class(pca_loading, "matrix")
  expect_true(inherits(pca_loading, what = "matrix"))
  expect_equal(colnames(pca_loading), expect_colnames)
  expect_equal(rownames(pca_loading), expect_rownames)

  # get_pca_loading on various arguments  ====

  #>> get_pca_loading on arguments: pc_name_prefix ----
  pca_res_prcomp <- prcomp(iris[, -5],  scale = TRUE)
  expect_loading <- pca_res_prcomp$rotation

  # Result from prcomp
  actual_loading <- get_pca_loading(pca_res_prcomp, pc_name_prefix = "PC")
  expect_equal(abs(actual_loading), abs(expect_loading), ignore_attr = TRUE)

  # Result from princomp
  pca_res_princomp <- princomp(scale(iris[, -5]))
  actual_loading <- get_pca_loading(pca_res_princomp,  pc_name_prefix = "PC")
  expect_equal(abs(actual_loading), abs(expect_loading), ignore_attr = TRUE)

  # Result from FactoMineR::PCA
  pca_res_PCA <- FactoMineR::PCA(iris[, -5], ncp = ncol(iris[, -5]), graph = FALSE)
  actual_loading <- get_pca_loading(pca_res_PCA,  pc_name_prefix = "PC")
  expect_equal(abs(actual_loading), abs(expect_loading), ignore_attr = TRUE)

  #>> get_pca_loading on arguments: ncp ----
  expect_pcs <- 2
  pca_loading <- get_pca_loading(pca_res, ncp = expect_pcs)
  expect_equal(NCOL(pca_loading), expect_pcs)

})

test_that("get_pca_formula, with various arguments", {

  pca_res <- prcomp(iris[, -5],  scale = TRUE)

  # get_pca_formula on default arguments  ====
  pca_formula <- get_pca_formula(pca_res)
  expect_s3_class(pca_formula, "character")
  expect_true(all(stringr::str_detect(pca_formula,
                                      "^\\$\\$Dim.+\\$\\$$")))

  # get_pca_formula on various arguments  ====
  #
  #>> get_pca_formula on arguments: pc_name_prefix ----
  pca_formula <- get_pca_formula(pca_res, pc_name_prefix = "PC")
  expect_s3_class(pca_formula, "character")
  expect_true(all(stringr::str_detect(pca_formula,
                                      "^\\$\\$PC.+\\$\\$$")))

  #>> get_pca_formula on arguments: ncp ----
  expect_pcs <- 2
  pca_formula <- get_pca_formula(pca_res, ncp = expect_pcs)
  expect_equal(length(pca_formula), expect_pcs)

  #>> get_pca_formula on arguments: digits ----
  expect_digits <- 2
  pca_formula <- get_pca_formula(pca_res, digits = expect_digits)
  pca_loading <- get_pca_loading(pca_res)
  # if coefficient format is correct,
  # coef_matrix should have same dimensions as pca_loading
  ceof_matrix <- stringr::str_extract_all(
    pca_formula,
    pattern = glue::glue("[0-9]+\\.[0-9]{{{expect_digits}}}"),
    simplify = TRUE
  )
  expect_equal(dim(ceof_matrix), dim(pca_loading))

})
