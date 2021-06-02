
# Skip tests if stock db is not ready
skip_if_stock_db_not_ready <- function(dsn = get_golem_config("database_dsn")) {

  # Test stock database is ready ?
  stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
  suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
  withr::defer({
    suppressMessages(zstmodelr::close_stock_db(stock_db))
  })

  # Skip if stock database is not ready
  testthat::skip_if_not(db_ready,
    message = sprintf(
      "DSN(%s) is not ready, skip tests related to stock database ",
      dsn
    )
  )
}
