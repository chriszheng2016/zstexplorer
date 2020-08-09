#' Run the Shiny Application
#'
#' @param app_module A character of app module to be launched.
#' @param ... A series of options to be used inside the app.
#'
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function( app_module= c("explore_factor"), ...) {

  app_module <-  match.arg(app_module)

  with_golem_options(
    {
      # Call main app
      app <- shinyApp(
        ui = app_ui,
        server = app_server
      )
    },
    golem_opts = list(app_module = app_module, ...)
  )
}
