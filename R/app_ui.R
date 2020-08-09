#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#'
#' @noRd
app_ui <- function(request) {

  app_module <- golem::get_golem_options("app_module")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      h1("zstexplorer"),
      if(!is.null(app_module)){
        switch(app_module,
               "explore_factor" = {
                 explore_factor_ui("explore_factor_module")
               }
        )
      },
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "zstexplorer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
