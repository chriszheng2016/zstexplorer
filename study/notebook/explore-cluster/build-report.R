# ---------------------------------
# Date:  2020-09-20 09:24:05
# Author:Chris Zheng
# Email: chrizheng@vip.sina.com.cn
# ---------------------------------
# project: zstexplorer

indcd_group <- c("C38", "C39")
cs_date <- "2018-12-31"

# Build report with parameters
zstmodelr::build_report("explore-cluster.Rmd",
  report_params = list(
    indcd_group = indcd_group,
    cs_date = cs_date
  ),
  output_sn = glue::glue(
    "{stringr::str_c(indcd_group,collapse = '_')}-{cs_date}"
  ),
  quiet = FALSE
)
