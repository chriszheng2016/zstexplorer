#' ---
#' title: "RUN SCRIPT: run_dev.R"
#' date:  "`r Sys.Date()`"
#' author: Chris Zheng
#' email: chrizheng@vip.sina.com.cn
#' output:
#'   html_document:lib
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
######################################
#### CURRENT FILE: RUN SCRIPT #####
######################################
#*/


#' # Run shiny app

#+ run_app, eval = FALSE
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
