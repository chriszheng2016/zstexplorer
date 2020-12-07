# context("Tests for functions about data service")

# Test database is ready ?
dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
suppressMessages(zstmodelr::close_stock_db(stock_db))
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for data service", dsn)
)

test_that("stock_db, with various arguments", {

  # stock_db with default arguments ====

  suppressMessages({
    stock_db <- stock_db()
    expect_true(!is.null(stock_db))
    expect_s4_class(stock_db, class = "gta_db")
    expect_gt(length(stock_db$stock_field_list), 0)
    expect_gt(length(stock_db$stock_name_list), 0)
    expect_gt(length(stock_db$industry_name_list), 0)
    expect_gt(length(stock_db$factor_name_list), 0)
    expect_gt(length(stock_db$indicator_name_list), 0)
  })
})

test_that("code2name/name2code, with various arguments", {

  # name2code/code2name with default arguments ====

  suppressMessages({
    # stock code/name
    expect_codes <- c("600031", "600030")
    expect_equal(name2code(code2name(expect_codes)), expect_codes)

    # industry code/name
    expect_codes <- c("C28", "C29")
    expect_equal(name2code(code2name(expect_codes)), expect_codes)

    # factor code/name
    expect_codes <- c("GPM", "OPM")
    expect_equal(name2code(code2name(expect_codes)), expect_codes)

    # indicator code/name
    expect_codes <- c("f050101b", "f050102b")
    expect_equal(name2code(code2name(expect_codes)), expect_codes)

    # non-exact match
    expect_gte(length(name2code(c("格力"))), 1)
    expect_gte(length(code2name(c("CR"))), 1)
  })

  # name2code/code2name with various arguments ====
  suppressMessages({
    # stock code/name
    expect_codes <- c("600031", "600030")
    expect_equal(
      name2code(code2name(expect_codes, exact_match = TRUE)),
      expect_codes
    )

    # industry code/name
    expect_codes <- c("C28", "C29")
    expect_equal(
      name2code(code2name(expect_codes, exact_match = TRUE), exact_match = TRUE),
      expect_codes
    )

    # factor code/name
    expect_codes <- c("GPM", "OPM")
    expect_equal(
      name2code(code2name(expect_codes, exact_match = TRUE), exact_match = TRUE),
      expect_codes
    )

    # indicator code/name
    expect_codes <- c("f050101b", "f050102b")
    expect_equal(
      name2code(code2name(expect_codes, exact_match = TRUE), exact_match = TRUE),
      expect_codes
    )

    # exact match
    expect_equal(length(name2code(c("格力"), exact_match = TRUE)), 1)
    expect_equal(length(code2name(c("CR"), exact_match = TRUE)), 1)
  })
})
