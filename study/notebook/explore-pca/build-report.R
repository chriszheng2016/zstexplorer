#' ---
#' title: Tool for build report
#' date:  2020-10-17 17:31:05
#' author: Chris Zheng
#' email: chrizheng@vip.sina.com.cn
#' ---



indcd_group <- c("C37", "C38", "C39")
cs_date <- "2018-12-31"

# Build report with parameters
zstmodelr::build_report("explore-pca.Rmd",
  report_params = list(
    indcd_group = indcd_group,
    cs_date = cs_date
  ),
  output_sn = glue::glue(
    "{stringr::str_c(indcd_group,collapse = '_')}-{cs_date}"
  ),
  quiet = FALSE
)
