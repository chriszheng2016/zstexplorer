# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("thinkr")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "name_of_module1") # Name of the module
golem::add_module(name = "name_of_module2") # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("helpers")
golem::add_utils("helpers")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Add module to dependency ----
usethis::use_package("package")
usethis::use_tidy_description()

## Style codes ----
usethis::use_tidy_style()

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")
devtools::wd("tests/testthat")
testthat::test_file("test-app.R") # test a file

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

# Document ----
#
# Update function doc
devtools::document()
?fun_abc

# Check spelling in doc
usethis::use_spell_check() # set up spelling check in doc
devtools::document() # update all doc
devtools::spell_check() # check spelling in doc
spelling::update_wordlist() # accept new words if need

# Vignette
usethis::use_vignette("zstexplorer")
devtools::build_vignettes()

## Check before merging ----

# Test package
devtools::test()

# Update doc and check spelling
devtools::document()
usethis::use_spell_check() # Set up spelling check in doc
devtools::spell_check()

# Check doc
devtools::check_man()

# R CMD Check
result <- devtools::check(
  cran = FALSE, args = c("--timings", "--no-tests"),
  check_dir = "check"
)
cat(result$errors)
cat(result$warnings)
cat(result$notes)

# Update document of pkgdown
devtools::build_site(quiet = FALSE)

## Code coverage ----
devtools::test_coverage()
usethis::use_coverage()

## Pkgdown site ----
usethis::use_pkgdown()
devtools::build_site()

## Setup CI if need ----
usethis::use_github()
usethis::use_tidy_github_actions()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
