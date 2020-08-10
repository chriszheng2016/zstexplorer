# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

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

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(name = "Chris Zheng") # You can set another license here
usethis::use_code_of_conduct()
usethis::use_news_md(open = FALSE)

# Add readme files
usethis::use_readme_rmd(open = FALSE)
usethis::use_lifecycle_badge("Experimental")
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)
usethis::use_git_ignore("*.html")
usethis::use_build_ignore("*.html")

## Set up git/github ----
# Set up git
usethis::use_git() # use git version control
usethis::use_git_protocol("https") # use https as transport protocol
usethis::git_sitrep()  # get current status of git setting

# Set github
usethis::use_github()  # set up initial repo in github
# add auxiliary files for tidy github project, Don't forget to describe the code
# of conduct in your README.rmd and render it again rmarkdown::render()
usethis::use_tidy_github()
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)


## Init Testing Infrastructure ----
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


# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
