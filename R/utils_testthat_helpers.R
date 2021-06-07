# Utility functions of helpers for testthat ----

# Skip tests if stock db is not ready

# This wrapper of [zstmodelr::skip_if_stock_db_not_ready()] to skip tests
# if stock db is not available for testing.
skip_if_stock_db_not_ready <- function(dsn = get_golem_config("database_dsn")) {
  zstmodelr::skip_if_stock_db_not_ready(dsn)
}
