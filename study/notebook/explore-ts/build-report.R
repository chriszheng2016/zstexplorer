#' ---
#' title: Tool for build report
#' date:  2020-10-17 17:31:05
#' author: Chris Zheng
#' email: chrizheng@vip.sina.com.cn
#' ---


focus_stocks <- c("格力电器", "美的集团")
start_date <- "2001-01-01"
end_date <- Sys.Date()

# Build report with parameters
zstmodelr::build_report("explore-ts-fpp3.Rmd",
  report_params = list(
    focus_stocks = focus_stocks,
    start_date = start_date,
    end_date = end_date
  ),
  output_sn = glue::glue(
    "{stringr::str_c(focus_stocks,collapse = '_')}-{start_date}-{end_date}"
  ),
  quiet = FALSE
)
