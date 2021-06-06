
# Skip tests if stock db is not ready
skip_if_stock_db_not_ready <- function(dsn = get_golem_config("database_dsn")) {

  # NO_STOCK_DB is used to simulate stock db is not unavailable
  no_stock_db <- identical(Sys.getenv("NO_STOCK_DB"), "true")

  if(!no_stock_db){
    # Test stock database is ready when stock db is avaliable
    stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
    suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
    withr::defer({
      suppressMessages(zstmodelr::close_stock_db(stock_db))
    })
  } else {
    db_ready <- FALSE
  }

  # Skip if stock database is not ready
  testthat::skip_if_not(db_ready,
    message = sprintf(
      "DSN(%s) is not ready, skip tests related to stock database ",
      dsn
    )
  )
}
