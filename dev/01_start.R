#' ---
#' title: "DEPLOY SCRIPT: 03_devply.R"
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
#*/

#' **README**: each step of the dev files is optional, and you don't have to
#' fill every dev scripts before getting started.
#'
#' * 01_start.R should be filled at start.
#'
#' * 02_dev.R should be used to keep track of your development during the project.
#'
#' * 03_deploy.R should be used once you need to deploy your app.
#'

#/*
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################
#*/

#' # Setup for R package project
#+ setup_r_pkg, eval=FALSE
devtools::load_all(".") # load pkg in development

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "zstexplorer", # The Name of the package containing the App
  pkg_title = "Explorer for China Stock Market Investment", # The Title of the package containing the App
  pkg_description = "An interactive tool to explore data of China stock market.", # The Description of the package containing the App
  author_first_name = "Chris", # Your First Name
  author_last_name = "Zheng", # Your Last Name
  author_email = "chriszheng@vip.sina.com.cn", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)
usethis::use_tidy_description() # tidy description

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "Chris Zheng")
usethis::use_code_of_conduct()
usethis::use_news_md(open = FALSE)

# Add readme files
usethis::use_readme_rmd(open = FALSE)
usethis::use_lifecycle_badge("Experimental")
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)
usethis::use_git_ignore("*.html")
usethis::use_build_ignore("*.html")

## Setup git/github ----
## Setup git
usethis::use_git() # use git version control
usethis::use_git_protocol("https") # use https as transport protocol
usethis::git_sitrep()  # get current status of git setting

## Setup github
usethis::use_github()  # set up initial repo in github
## Add auxiliary files for tidy github project
usethis::use_tidy_github()
## Don't forget to render it again rmarkdown::render()
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)



## Setup CI if need ----

## Setup Github actions for CI

# Method A: all-in-one setup
## 1. Run R CMD check on the current release, devel, and four previous versions of R.
## 2. Report test coverage.
## 3. Build and deploy a pkgdown site.
## 4. Provide two commands to be used in pull requests:
##   /document to run roxygen2::roxygenise() and update the PR, and
##   /style to run styler::style_pkg() and update the PR
usethis::use_tidy_github_actions()


## Method B: individual setup

## R CMD check
## Check on the latest release of R on macOS
usethis::use_github_action_check_release()

## Check on the three major operating systems
## (linux, macOS, and Windows) on the latest release of R and on R-devel.
usethis::use_github_action_check_standard()

## Check on at least once on each of the three major operating systems
## (linux, macOS, and Windows) and on the current release, devel,
## and four previous versions of R.
usethis::use_github_action_check_full()

## Set PR actions
## This workflow enables the use of two R-specific commands in pull request issue comments:
## * /document to run roxygen2::roxygenise() and update the PR
## * /style to run styler::style_pkg() and update the PR
usethis::use_github_action_pr_commands()

## Set code coverage report
usethis::use_coverage()

## Set pkgdown site
usethis::use_pkgdown()

## Don't forget to render it again rmarkdown::render()
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)

## Setup other CI facilities
usethis::use_travis()
usethis::use_appveyor()


#' # Setup for shiny application project
#+ setup_shiny_app, eval=FALSE

## Set {golem} options ----
golem::set_golem_options()

## Initialize Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon() # path = "path/to/ico". Can be an online file.

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()


#/*
# You're now set! ----
# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
#*/
