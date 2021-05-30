#' ---
#' title: "DEV SCRIPT: 03_dev.R"
#' date:  "`r Sys.Date()`"
#' author: Chris Zheng
#' email: chrizheng@vip.sina.com.cn
#' output:
#'   html_document:
#'      fig_caption: yes
#'      number_sections: yes
#'      theme: cerulean
#'      highlight: pygments
#' ---

#/*
# Building a Prod-Ready, Robust Shiny Application.
#
#
# **README**: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
#
# * 01_start.R should be filled at start.
#
# * 02_dev.R should be used to keep track of your development during the project.
#
# * 03_deploy.R should be used once you need to deploy your app.
#
# * run_dev.R should be used to run your app in production/debug mode
#
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################
#*/


#' # Develop shiny app

#+ develop_app, eval=FALSE

## Add package to dependency ----
## Add one line by package you want to add as dependency
usethis::use_package("package")
usethis::use_tidy_description()

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "name_of_module1") # Name of the module
golem::add_module(name = "name_of_module2") # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("helpers")
golem::add_utils("helpers")

## Add external resources ----
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")

## Add customized shiny module for zstexplorer package ----

## Create general shiny module for zstexplorer
#Create source file for module of "xyz" with name of "mod_xyz.R"
add_shiny_module("xyz")
# Try integration test of module
devtools::load_all()
xyz_app()
# Create test file for module of "xyz" with name of "test-mod_zys.R"
add_shiny_module("xyz", type = "test")
# Make sure tests of module is OK
test_file("tests/testthat/test-mod_xyz.R")

## Create cross-section analysis module for zstexplorer
# Create source and test file for module of "cs_xyz" with name of "mod_cs_xyz.R"
# and "test-mod_cs_xyz.R"
add_cs_module("xyz")
# Try integration test of module
devtools::load_all()
xyz_cs_app()
# Make sure tests of module is OK
test_file("tests/testthat/test-mod_cs_xyz.R")

## Create time series analysis module for zstexplorer
# Create source and test file for module of "ts_xyz" with name of "mod_ts_xyz.R"
# and "test-mod_ts_xyz.R"
add_ts_module("xyz")
# Try integration test of module
devtools::load_all()
xyz_ts_app()
# Make sure tests of module is OK
devtools::test_file("tests/testthat/test-mod_ts_xyz.R")

## Enable/disable debug for zstexplorer ----
devtools::load_all()
enable_debug()  # Enable environment variable for debug
disable_debug() # disable environment variable for debug
on_debug() # Judge whether debug is enable or not
save_debug_data(output_data, output_file) # Save data for debug in app/temp dir

## Style codes ----
usethis::use_tidy_style()

## Tools for plot ----

# Returns the built-in color names which R knows about.
colors()
# Display similar colors
plotCol(nearRcolor("skyblue", dist=.1))


## Tests ----
## Add one line by test you want to create
usethis::use_test("app")
devtools::wd("tests/testthat")
testthat::test_file("test-app.R") # test a file
devtools::test_file_coverage("test-app.R") # test a file coverage

# Test a file which contains "skip_on_cran()/skip_on_ci()/skip_on_covr()"
# Method A:
withr::with_envvar(
  new = c("NOT_CRAN" = "true",
          "CI" = "false",
          "R_COVR" = "false"),
  testthat::test_file("test-app.R")
)
# Method B:
Sys.setenv("NOT_CRAN" = "true")
testthat::test_file("test-app.R")

devtools::test() # test package

## Document ----

## Update function doc
devtools::document()
?fun_abc

## Check spelling in doc
usethis::use_spell_check() # set up spelling check in doc
devtools::document() # update all doc
devtools::spell_check() # check spelling in doc
spelling::update_wordlist() # accept new words if need

## Edit vignette
usethis::use_vignette("zstexplorer")
devtools::build_vignettes()

## Check before merging ----

## Test package
devtools::test()

## Test coverage
devtools::test_coverage()

## Style package
## Styles source code according to the tidyverse style guide.
usethis::use_tidy_style() ## It will overwrite files!

## Update doc and check spelling
devtools::document()
usethis::use_spell_check() # Set up spelling check in doc
devtools::spell_check()

## Check doc
devtools::check_man()

## R CMD Check
result <- devtools::check(
  cran = FALSE, args = c("--timings", "--no-tests"),
  check_dir = "check"
)
cat(result$errors)
cat(result$warnings)
cat(result$notes)

# Update document of pkgdown
devtools::build_site(quiet = FALSE)


#/*
# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
#*/
