#' cs_PCA_FactoMineR
#'
#' @description A shiny module for cs_PCA_FactoMineR.
#'
#' @details
#'  The module is an UI for user to display plots of PCA
#'  by [`FactoMineR`][FactoMineR::FactoMineR] and
#'  `factoextra` package.
#'
#' @name cs_PCA_FactoMineR
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_PCA_FactoMineR_ui("cs_PCA_FactoMineR_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_PCA_FactoMineR <- cs_PCA_FactoMineR_server(
#'     "cs_PCA_FactoMineR_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_PCA_FactoMineR_app()
#' }
#'
NULL

#' UI function of cs_PCA_FactoMineR
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_PCA_FactoMineR  UI function of cs_PCA_FactoMineR.
#' @importFrom shiny NS tagList
cs_PCA_FactoMineR_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    withMathJax(),
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        shinyjs::hidden(
          selectInput(
            inputId = ns("plot_engine"),
            label = strong("Plot by:"),
            choices = c("factoextra", "FactoMineR")
          )
        ),
        tabsetPanel(
          id = ns("plot_setting"),
          type = "hidden",
          tabPanelBody(
            value = "compute_pca",

            tabsetPanel(
              id = ns("compute_pca_setting"),
              type = "tabs",
              tabPanel(
                "Parameters",
                selectInput(
                  inputId = ns("sup_quanti_vars"),
                  label = strong("Supplement quantitative vars:"),
                  choices = c("")
                ),
                selectInput(
                  inputId = ns("sup_quali_vars"),
                  label = strong("Supplement categorical vars:"),
                  choices = c("")
                ),
                selectInput(
                  inputId = ns("sup_inds"),
                  label = strong("Supplement individuals:"),
                  choices = c("")
                )
              ),
              tabPanel(
                "Missing Values",
                textOutput(ns("missing_status")),
                br(),
                selectInput(
                  inputId = ns("handle_miss_method"),
                  label = strong("Handle missing method:"),
                  choices = c("imputePCA", "na.omit")
                )
              )
            ),

            actionButton(
              inputId = ns("comput_pca"),
              label = strong("Compute PCA")
            )
          ),
          tabPanelBody(
            value = "inertia_distribution",

            selectInput(
              inputId = ns("inertia_fviz_choice"),
              label = strong("Value to show:"),
              choices = c("variance", "eigenvalue")
            ),
            selectInput(
              inputId = ns("inertia_fviz_geom"),
              label = strong("geom to show:"),
              choices = c("bar", "line")
            ),
            actionButton(
              inputId = ns("analyze_inertia"),
              label = strong("Analyze Intertia")
            )
          ),
          tabPanelBody(
            value = "var_ind_plane",

            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = ns("pc_x"),
                  label = strong("PC on x:"),
                  choices = as.character(1:4),
                  selected = "1"
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = ns("pc_y"),
                  label = strong("PC on y:"),
                  choices = as.character(1:4),
                  selected = "2"
                )
              ),
            ),
            selectInput(
              inputId = ns("plot_show_detail"),
              label = strong("Show detail:"),
              choices = c("Cos2", "Contribution")
            ),

            checkboxInput(
              inputId = ns("plot_show_legend"),
              label = "Show legend",
              value = TRUE
            ),

            checkboxGroupInput(
              inputId = ns("draw_element"),
              label = strong("Elements to draw"),
              choices = list(
                "Individuals" = "ind",
                "Supplementary individuals" = "ind.sup",
                "Supplementary categories" = "quali",
                "active variables" = "var",
                "quantitative supplementary variables" = "quanti.sup"
              ),
              selected = c("ind", "ind.sup", "quali", "var", "quanti.sup")
            ),

            wellPanel(
              selectInput(
                inputId = ns("ind_color_point"),
                label = strong("Colour points according to:"),
                choices = c("qualitative variable", "active/supplementary")
              ),

              conditionalPanel(
                condition = "input.ind_color_point == 'qualitative variable'",

                selectInput(
                  inputId = ns("ind_color_point_quali_var"),
                  label = strong("select the variable:"),
                  choices = ""
                ),

                checkboxInput(
                  inputId = ns("ind_color_point_quali_ellipses"),
                  label = "Draw the confidence ellipses around the categories",
                  value = FALSE
                ),

                ns = ns
              ),

              conditionalPanel(
                condition = "input.ind_color_point == 'active/supplementary'",

                colourpicker::colourInput(
                  inputId = ns("ind_color_point_active_individual"),
                  label = strong("active individuals:"),
                  value = "black"
                ),

                colourpicker::colourInput(
                  inputId = ns("ind_color_point_sup_individual"),
                  label = strong("supplementary individuals:"),
                  value = "blue"
                ),

                colourpicker::colourInput(
                  inputId = ns("ind_color_point_quali_category"),
                  label = strong("categories:"),
                  value = "magenta"
                ),

                ns = ns
              )
            ),

            checkboxGroupInput(
              inputId = ns("label_element"),
              label = strong("Label for element"),
              choices = list(
                "Individuals" = "ind",
                "Supplementary individuals" = "ind.sup",
                "Supplementary categories" = "quali",
                "active variables" = "var",
                "quantitative supplementary variables" = "quanti.sup"
              ),
              selected = c("ind", "ind.sup", "quali", "var", "quanti.sup")
            ),

            wellPanel(
              selectInput(
                inputId = ns("var_select_label"),
                label = strong("Labels for variables selected by:"),
                choices = c("No selection", "Cos2", "Contribution")
              ),

              conditionalPanel(
                condition = "input.var_select_label == 'Cos2'",
                sliderInput(
                  inputId = ns("var_select_label_cos2"),
                  label = strong("Labels for cos2 greater than"),
                  min = 0,
                  max = 1,
                  value = 0,
                  step = NULL
                ),
                p("'0': select optimal numbers of varables with highest cos2"),
                ns = ns
              ),
              conditionalPanel(
                condition = "input.var_select_label == 'Contribution'",
                sliderInput(
                  inputId = ns("var_select_label_contrib"),
                  label = strong("Number of the most contributive varables"),
                  min = 0,
                  max = 50,
                  value = 0,
                  step = NULL
                ),
                p("'0': select optimal numbers of varables with highest contribution"),
                ns = ns
              )
            ),

            wellPanel(
              selectInput(
                inputId = ns("ind_select_label"),
                label = strong("Labels for individuals selected by:"),
                choices = c("No selection", "Cos2", "Contribution")
              ),

              conditionalPanel(
                condition = "input.ind_select_label == 'Cos2'",
                wellPanel(
                  sliderInput(
                    inputId = ns("ind_select_label_cos2"),
                    label = strong("Labels for cos2 greater than"),
                    min = 0,
                    max = 1,
                    value = 0,
                    step = NULL
                  ),
                  p("'0': select optimal numbers of individuals with highest cos2"),
                ),
                ns = ns
              ),
              conditionalPanel(
                condition = "input.ind_select_label == 'Contribution'",
                wellPanel(
                  sliderInput(
                    inputId = ns("ind_select_label_contrib"),
                    label = strong("Number of the most contributive individuals"),
                    min = 0,
                    max = 50,
                    value = 0,
                    step = NULL
                  ),
                  p("'0': select optimal numbers of individuals with highest contribution"),
                ),
                ns = ns
              ),
            ),
          ),
          tabPanelBody(
            value = "classfication_hcpc",

            conditionalPanel(
              condition = "input.plot_engine == 'factoextra'",
              selectInput(
                inputId = ns("classfication_hcpc_plot_factoextra"),
                label = strong("Plot to drawn:"),
                choices = c("cluster", "dendrogram")
              ),

              ns = ns
            ),

            conditionalPanel(
              condition = "input.plot_engine == 'FactoMineR'",
              selectInput(
                inputId = ns("classfication_hcpc_plot_FactoMineR"),
                label = strong("Plot to drawn:"),
                choices = c("tree", "bar", "map", "3D.map")
              ),

              ns = ns
            ),

            conditionalPanel(
              condition = "(input.plot_engine == 'factoextra')&
                  (input.classfication_hcpc_plot_factoextra == 'dendrogram')",
              selectInput(
                inputId = ns("classfication_hcpc_plot_factoextra_dendrogram_type"),
                label = strong("dendrogram type:"),
                choices = c("rectangle", "circular", "phylogenic"),
              ),

              ns = ns
            ),

            checkboxInput(
              inputId = ns("classfication_hcpc_show_label"),
              label = "Show lablel",
              value = TRUE
            ),
          )
        )
      ),
      mainPanel(
        width = 9,

        tabsetPanel(
          id = ns("plot_tabs"),
          type = "tabs",
          tabPanel(
            "Compute PCA",

            tabsetPanel(
              id = ns("pca_res"),
              type = "pills",
              # tabBox(
              #   id = ns("pca_res"),
              #   width = 12,
              tabPanel(
                "Summary of PCA",
                div(
                  # style = "width:200px;
                  #   overflow-x: scroll;
                  #   height:200px;
                  #   overflow-y: scroll;",
                  style = "height:700px;
                    overflow-y: scroll;",
                  verbatimTextOutput(ns("pca_result_summary"))
                )
              ),
              tabPanel(
                "Formula of PCs",
                uiOutput(ns("pca_result_formula"))
              )
            )
          ),
          tabPanel(
            "Inertia distribution",
            box(
              title = "Inertia description", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              width = 12,
              uiOutput(ns("inertia_desc")),
            ),
            fluidRow(
              box(
                title = "Scree plot ", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                plotOutput(ns("inertia_plot"))
              ),
              box(
                title = "Eigenvalues", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                tableOutput(ns("inertia_table"))
              )
            ),
            box(
              title = "Analysis of Inertial Distribution", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              width = 12,
              uiOutput(ns("inertia_analysis"))
            )
          ),
          tabPanel(
            "Variables Graph",
            fluidRow(
              box(
                title = "Varables factor map", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                plotOutput(ns("var_factor_map"))
              ),
              box(
                title = "Description", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                div(
                  # style = "width:200px;
                  #   overflow-x: scroll;
                  #   height:200px;
                  #   overflow-y: scroll;",
                  style = "height:400px;
                    overflow-y: scroll;",
                  uiOutput(ns("var_plot_desc"))
                )
              )
            ),
            fluidRow(
              box(
                title = "Bar plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                width = 6,
                plotOutput(ns("var_bar_plot"))
              ),
              box(
                title = "Matrix plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                width = 6,
                plotOutput(ns("var_matrix_plot"))
              )
            ),
          ),
          tabPanel(
            "Individuals Graph",

            fluidRow(
              box(
                title = "Individuals factor map", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                plotOutput(ns("ind_factor_map"))
              ),
              box(
                title = "Description", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                div(
                  # style = "width:200px;
                  #   overflow-x: scroll;
                  #   height:200px;
                  #   overflow-y: scroll;",
                  style = "height:400px;
                    overflow-y: scroll;",
                  uiOutput(ns("ind_plot_desc"))
                )
              )
            ),
            fluidRow(
              box(
                title = "Bar plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                width = 6,
                plotOutput(ns("ind_bar_plot"))
              ),
              box(
                title = "Matrix plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                width = 6,
                plotOutput(ns("ind_matrix_plot"))
              )
            ),
          ),
          tabPanel(
            "Plane Description",

            box(
              title = "Joint factor map", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              width = 12,
              plotOutput(ns("plane_joint_factor_map"))
            ),

            box(
              title = "Joint factor description", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              width = 12,
              div(
                # style = "width:200px;
                #   overflow-x: scroll;
                #   height:200px;
                #   overflow-y: scroll;",
                style = "height:420px;
                    overflow-y: scroll;",
                uiOutput(ns("plane_joint_factor_desc"))
              )
            )
          ),
          tabPanel(
            "Classification(HCPC)",

            fluidRow(
              box(
                title = "Clusters plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,
                plotOutput(ns("hcpc_cluster_plot"))
              ),
              box(
                title = "Clusters Decription", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                width = 6,

                tabsetPanel(
                  id = ns("hcpc_cluster_desc"),
                  type = "tabs",
                  tabPanel(
                    "Overall",
                    div(
                      style = "height:355px;
                      overflow-y: scroll;",
                      uiOutput(ns("hcpc_cluster_desc_overall"))
                    )
                  ),
                  tabPanel(
                    "By varables",
                    div(
                      style = "height:355px;
                      overflow-y: scroll;",
                      verbatimTextOutput(ns("hcpc_cluster_desc_by_var"))
                    )
                  ),
                  tabPanel(
                    "By dimensions",
                    div(
                      style = "height:355px;
                      overflow-y: scroll;",
                      verbatimTextOutput(ns("hcpc_cluster_desc_by_dim"))
                    )
                  ),
                  tabPanel(
                    "By individuals",
                    div(
                      style = "height:355px;
                      overflow-y: scroll;",
                      verbatimTextOutput(ns("hcpc_cluster_desc_by_ind"))
                    )
                  )
                )
              )
            ),
            box(
              title = "Clustering Result", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              width = 12,
              # tabsetPanel(
              tabBox(
                id = ns("hcpc_cluster_table"),
                width = 12,
                # type = "pills",
                tabPanel(
                  "Clustering Results",
                  DT::dataTableOutput(ns("hcpc_cluster_table_result"))
                ),
                tabPanel(
                  "Representiative individuals",
                  DT::dataTableOutput(ns("hcpc_cluster_table_rep_ind"))
                ),
                tabPanel(
                  "Clustering Mapping",
                  DT::dataTableOutput(ns("hcpc_cluster_table_mapping"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Server function of cs_PCA_FactoMineR
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_PCA_FactoMineR  Server function of cs_PCA_FactoMineR.
#' @return * Server function return a data frame of ...
cs_PCA_FactoMineR_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Logic reactive ----

    # Raw dataset
    df_pca_raw <- reactive({
      csbl_vars() %>%
        tidyr::separate(.data$id,
          into = c("date", "period", "stkcd"), sep = "_"
        ) %>%
        dplyr::select(-c("date", "period")) %>%
        # Use median for numeric field of multiple period series of stock
        # Use mode numbers for non-numeric filed of multiple period series of stock
        dplyr::group_by(.data$stkcd) %>%
        dplyr::summarise(
          indcd = names(which.max(table(.data$indcd))),
          dplyr::across(where(is.numeric), median),
          .groups = "drop"
        ) %>%
        # Scale data
        dplyr::mutate(dplyr::across(where(is.numeric), scale)) %>%
        tibble::column_to_rownames(var = "stkcd")
    })

    # Missing value in Raw dataset
    missing_status_pca_raw <- reactive({
      missing_status <- purrr::map_dbl(df_pca_raw(), ~ sum(is.na(.x)))
      missing_status
    })

    # Dataset for PCA
    df_pca <- reactive({

      # Only keep supplementary qualitative vars and numeric vars
      numeric_vars <- zstmodelr::expect_type_fields(
        df_pca_raw(),
        expect_type = "numeric"
      )
      if (input$sup_quali_vars == "") {
        df_pca <- df_pca_raw() %>%
          dplyr::select(numeric_vars)
      } else {
        df_pca <- df_pca_raw() %>%
          dplyr::select(c(input$sup_quali_vars, numeric_vars))
      }

      # Deal with missing value if need
      has_missing <- any(missing_status_pca_raw() != 0)
      if (has_missing) {
        switch(isolate(input$handle_miss_method),
          "imputePCA" = {
            if (input$sup_quanti_vars == "") {
              index_quanti_sup <- NULL
            } else {
              index_quanti_sup <- grep(input$sup_quanti_vars, names(df_pca))
              if (length(index_quanti_sup) == 0) {
                index_quanti_sup <- NULL
              }
            }

            if (input$sup_quali_vars == "") {
              index_quali_sup <- NULL
            } else {
              index_quali_sup <- grep(input$sup_quali_vars, names(df_pca))
              if (length(index_quali_sup) == 0) {
                index_quali_sup <- NULL
              }
            }

            if (input$sup_inds == "") {
              index_ind_sup <- NULL
            } else {
              index_ind_sup <- grep(input$sup_inds, rownames(df_pca))
              if (length(index_ind_sup) == 0) {
                index_ind_sup <- NULL
              }
            }

            estimate_ncp_PCA <- missMDA::estim_ncpPCA(
              df_pca,
              method.cv = "gcv",
              quanti.sup = index_quanti_sup,
              quali.sup = index_quali_sup,
              ind.sup = index_ind_sup
            )

            impute_res <- missMDA::imputePCA(
              df_pca,
              ncp = estimate_ncp_PCA$ncp,
              quanti.sup = index_quanti_sup,
              quali.sup = index_quali_sup,
              ind.sup = index_ind_sup
            )

            df_pca <- impute_res$completeObs
          },
          "na.omit" = {
            df_pca <- na.omit(df_pca)
          }
        )
      }

      df_pca
    })

    # Compute pca
    pca_res <- eventReactive(input$comput_pca, {
      notice_id <- showNotification(
        "Compute PCA, it might take a while...",
        duration = NULL
      )

      if (input$sup_quanti_vars == "") {
        index_quanti_sup <- NULL
      } else {
        index_quanti_sup <- grep(input$sup_quanti_vars, names(df_pca()))
        if (length(index_quanti_sup) == 0) {
          index_quanti_sup <- NULL
        }
      }

      if (input$sup_quali_vars == "") {
        index_quali_sup <- NULL
      } else {
        index_quali_sup <- grep(input$sup_quali_vars, names(df_pca()))
        if (length(index_quali_sup) == 0) {
          index_quali_sup <- NULL
        }
      }

      if (input$sup_inds == "") {
        index_ind_sup <- NULL
      } else {
        index_ind_sup <- grep(input$sup_inds, rownames(df_pca()))
        if (length(index_ind_sup) == 0) {
          index_ind_sup <- NULL
        }
      }

      pca_res <- FactoMineR::PCA(
        df_pca(),
        graph = FALSE,
        quanti.sup = index_quanti_sup,
        quali.sup = index_quali_sup,
        ind.sup = index_ind_sup
      )

      removeNotification(notice_id)

      pca_res
    })

    # Current PCs on axes
    pc_axes <- reactive({
      req(input$pc_x != input$pc_y)
      pc_axes <- c(
        as.numeric(input$pc_x),
        as.numeric(input$pc_y)
      )
    })


    # Focus varaiables to show
    focus_vars <- reactive({
      switch(input$var_select_label,
        "Cos2" = {
          if (input$var_select_label_cos2 == 0) {
            # Select optimal numbers of variables with the highest cos2 on
            # the 2 dimensions of the plane
            select_element <- "cos2"
          } else {
            # Select the variables that have a cos2 higher than the
            # specified threshold
            select_element <- glue::glue("cos2 {input$var_select_label_cos2}")
          }

          select_vars <- FactoInvestigate::selection(
            pca_res(),
            dim = pc_axes(),
            margin = 2, # computes on the active variables
            selec = select_element
          )
        },
        "Contribution" = {
          if (input$var_select_label_contrib == 0) {
            # Select optimal numbers of variables with highest contribution on
            # the 2 dimensions of the plane
            select_element <- "contrib"
          } else {
            # Select top n active or illustrative elements that have the highest
            # contribution on the 2 dimensions of the plane
            select_element <- glue::glue("contrib {input$var_select_label_contrib}")
          }

          select_vars <- FactoInvestigate::selection(
            pca_res(),
            dim = pc_axes(),
            margin = 2, # computes on the active variables
            selec = select_element
          )
        },
        "No selection" = {
          select_vars <- NULL
        }
      )

      select_vars
    })

    # Focus individuals to dhow
    focus_inds <- reactive({
      select_inds <- NULL
      switch(input$ind_select_label,
        "Cos2" = {
          if (input$ind_select_label_cos2 == 0) {
            # Select optimal numbers of variables with the highest cos2 on
            # the 2 dimensions of the plane
            select_element <- "cos2"
          } else {
            # Select the variables that have a cos2 higher than the
            # specified threshold
            select_element <- glue::glue("cos2 {input$ind_select_label_cos2}")
          }

          select_inds <- FactoInvestigate::selection(
            pca_res(),
            dim = pc_axes(),
            margin = 1, # computes on the individuals
            selec = select_element
          )
        },
        "Contribution" = {
          if (input$ind_select_label_contrib == 0) {
            # Select optimal numbers of variables with highest contribution on
            # the 2 dimensions of the plane
            select_element <- "contrib"
          } else {
            # Select top n active or illustrative elements that have the highest
            # contribution on the 2 dimensions of the plane
            select_element <- glue::glue("contrib {input$ind_select_label_contrib}")
          }

          select_inds <- FactoInvestigate::selection(
            pca_res(),
            dim = pc_axes(),
            margin = 1, # computes on the individuals
            selec = select_element
          )
        },
        "No selection" = {
          select_inds <- NULL
        }
      )

      select_inds
    })

    # Compute HCPC
    hcpc_res <- reactive({
      hcpc_res <- FactoMineR::HCPC(pca_res(), nb.clust = -1, graph = FALSE)
      hcpc_res
    })

    # Clusters result from from HCPC computation
    cluster_result_hcpc <- reactive({
      cluster_result_hcpc <- hcpc_res()$data.clust %>%
        tibble::rownames_to_column(var = "stkcd") %>%
        dplyr::mutate(
          indcd = as.character(.data$indcd),
          indname = code2name(.data$indcd),
          stkname = code2name(.data$stkcd),
          cluster = as.factor(.data$clust)
        ) %>%
        dplyr::select(
          c("cluster", "stkcd", "stkname", "indcd", "indname"),
          -c("clust"),
          tidyselect::everything()
        )

      cluster_result_hcpc
    })

    # Representative individuals of each cluster
    rep_inds_hcpc <- reactive({
      rep_inds_hcpc <- purrr::map(hcpc_res()$desc.ind$para, ~ names(.x))
      rep_inds_hcpc <- purrr::reduce(rep_inds_hcpc, .f = c)

      rep_inds_hcpc
    })


    # User control interaction ----

    # Update UI with dataset and user interaction
    observe({
      numeric_vars <- zstmodelr::expect_type_fields(
        df_pca_raw(),
        expect_type = "numeric"
      )
      category_vars <- c(
        zstmodelr::expect_type_fields(
          df_pca_raw(),
          expect_type = c("character")
        ), zstmodelr::expect_type_fields(
          df_pca_raw(),
          expect_type = c("factor")
        )
      )

      ind_ids <- rownames(df_pca_raw())

      updateSelectInput(
        session = session, inputId = "sup_quanti_vars",
        choices = c("", numeric_vars)
      )

      updateSelectInput(
        session = session, inputId = "sup_quali_vars",
        # choices = c(category_vars, "")
        choices = category_vars
      )

      updateSelectInput(
        session = session, inputId = "sup_inds",
        choices = c("", ind_ids)
      )

      pcs <- as.character(1:4)
      y_pcs <- setdiff(pcs, input$pc_x)
      updateSelectInput(
        session = session, inputId = "pc_y",
        choices = y_pcs,
        selected = input$pc_y
      )

      # Use qauli vars in pcs_res as choice instead of category_vars in df_pca
      pca_res_qauli_vars <- names(pca_res()$call$quali.sup$quali.sup)
      updateSelectInput(
        session = session, inputId = "ind_color_point_quali_var",
        choices = pca_res_qauli_vars
      )
    })


    # Update setting UI when user choose plot tabs
    observeEvent(input$plot_tabs, ignoreInit = TRUE, {

      # Update setting_tabs according to plot type
      switch(req(input$plot_tabs),
        "Compute PCA" = {
          shinyjs::hide(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "compute_pca"
          )
        },
        "Inertia distribution" = {
          shinyjs::hide(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "inertia_distribution"
          )
        },
        "Variables Graph" = {
          shinyjs::show(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "var_ind_plane"
          )

          updateCheckboxGroupInput(session,
            inputId = "draw_element",
            choices = list(
              "active variables" = "var",
              "quantitative supplementary variables" = "quanti.sup"
            ),
            selected = c("var", "quanti.sup")
          )

          updateCheckboxGroupInput(session,
            inputId = "label_element",
            choices = list(
              "active variables" = "var",
              "quantitative supplementary variables" = "quanti.sup"
            ),
            selected = c("var", "quanti.sup")
          )

          shinyjs::hide(id = "ind_color_point")
          shinyjs::hide(id = "ind_color_point_quali_var")
          shinyjs::hide(id = "ind_color_point_quali_ellipses")
          shinyjs::hide(id = "ind_color_point_active_individual")
          shinyjs::hide(id = "ind_color_point_sup_individual")
          shinyjs::hide(id = "ind_color_point_quali_category")
          shinyjs::hide(id = "ind_select_label")
          shinyjs::hide(id = "ind_select_label_cos2")
          shinyjs::hide(id = "ind_select_label_contrib")

          shinyjs::show(id = "var_select_label")
          shinyjs::show(id = "var_select_label_cos2")
          shinyjs::show(id = "var_select_label_contrib")

          shinyjs::show(id = "plot_show_detail")
        },
        "Individuals Graph" = {
          shinyjs::show(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "var_ind_plane"
          )

          updateCheckboxGroupInput(session,
            inputId = "draw_element",
            choices = list(
              "Individuals" = "ind",
              "Supplementary individuals" = "ind.sup",
              "Supplementary categories" = "quali"
            ),
            selected = c("ind", "ind.sup", "quali")
          )

          updateCheckboxGroupInput(session,
            inputId = "label_element",
            choices = list(
              "Individuals" = "ind",
              "Supplementary individuals" = "ind.sup",
              "Supplementary categories" = "quali"
            ),
            selected = c("ind", "ind.sup", "quali")
          )

          shinyjs::show(id = "ind_color_point")
          shinyjs::show(id = "ind_color_point_quali_var")
          shinyjs::show(id = "ind_color_point_quali_ellipses")
          shinyjs::show(id = "ind_color_point_active_individual")
          shinyjs::show(id = "ind_color_point_sup_individual")
          shinyjs::show(id = "ind_color_point_quali_category")
          shinyjs::show(id = "ind_select_label")
          shinyjs::show(id = "ind_select_label_cos2")
          shinyjs::show(id = "ind_select_label_contrib")


          shinyjs::hide(id = "var_select_label")
          shinyjs::hide(id = "var_select_label_cos2")
          shinyjs::hide(id = "var_select_label_contrib")

          shinyjs::show(id = "plot_show_detail")
        },
        "Plane Description" = {
          shinyjs::hide(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "var_ind_plane"
          )

          updateCheckboxGroupInput(session,
            inputId = "draw_element",
            choices = list(
              "Individuals" = "ind",
              "Supplementary individuals" = "ind.sup",
              "Supplementary categories" = "quali",
              "active variables" = "var",
              "quantitative supplementary variables" = "quanti.sup"
            ),
            selected = c("ind", "ind.sup", "quali", "var", "quanti.sup")
          )

          updateCheckboxGroupInput(session,
            inputId = "label_element",
            choices = list(
              "Individuals" = "ind",
              "Supplementary individuals" = "ind.sup",
              "Supplementary categories" = "quali",
              "active variables" = "var",
              "quantitative supplementary variables" = "quanti.sup"
            ),
            selected = c("ind", "ind.sup", "quali", "var", "quanti.sup")
          )

          shinyjs::show(id = "ind_color_point")
          shinyjs::show(id = "ind_color_point_quali_var")
          shinyjs::show(id = "ind_color_point_quali_ellipses")
          shinyjs::show(id = "ind_color_point_active_individual")
          shinyjs::show(id = "ind_color_point_sup_individual")
          shinyjs::show(id = "ind_color_point_quali_category")
          shinyjs::show(id = "ind_select_label")
          shinyjs::show(id = "ind_select_label_cos2")
          shinyjs::show(id = "ind_select_label_contrib")

          shinyjs::show(id = "var_select_label")
          shinyjs::show(id = "var_select_label_cos2")
          shinyjs::show(id = "var_select_label_contrib")

          shinyjs::hide(id = "plot_show_detail")
        },
        "Classification(HCPC)" = {
          shinyjs::show(id = "plot_engine")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "classfication_hcpc"
          )
        }
      )
    })

    # Render output ----

    # > PCA compuation ----
    output$missing_status <- renderText({
      has_missing <- any(missing_status_pca_raw() != 0)
      if (has_missing) {
        vars_with_missing <- names(df_pca_raw())[missing_status_pca_raw() != 0]
        miss_status_string <-
          glue::glue(
            "Some missing values in {glue::glue_collapse(vars_with_missing, sep = ',')}"
          )
      } else {
        miss_status_string <- "No missing values"
      }
    })

    output$pca_result_summary <- renderPrint({
      pca_res <- pca_res()

      # General summary of pca results
      summary(pca_res)

      # Additional Information about loading/rotation
      cat("\nLoadings/rotation\n")
      get_pca_loading(pca_res)
    })

    output$pca_result_formula <- renderUI({

      # Get formula of primary components of PCA
      pca_formula <- get_pca_formula(pca_res(),
        pc_name_prefix = "PC",
        digits = 2
      )

      pca_formula_mathJax <- purrr::map(pca_formula, .f = helpText)

      withMathJax(
        pca_formula_mathJax
      )
    })

    # > Inertia Distribution ----
    output$inertia_desc <- renderUI({

      # Load description file for inertia distribution
      desc_file <- app_sys(
        "app/ui/mod_cs_PCA_FactoMineR/inertia_desc.md"
      )
      includeMarkdown(desc_file)
    })


    output$inertia_plot <- renderPlot({
      factoextra::fviz_eig(
        pca_res(),
        choice = input$inertia_fviz_choice,
        geom = input$inertia_fviz_geom,
        addlabels = TRUE
      ) +
        ggplot2::labs(subtitle = "compute by PCA()")
    })

    output$inertia_table <- renderTable({
      eig_res <- factoextra::get_eig(pca_res())
      eig_res
    })

    output$inertia_analysis <- renderUI({
      req(input$analyze_inertia)

      notice_id <- showNotification(
        "Analyze inertia distribution, it might take a while...",
        duration = NULL
      )

      inerital_res_string <- utils::capture.output({
        invisible(
          best_ncp <- FactoInvestigate::inertiaDistrib(
            pca_res(),
            q = 0.95, time = "10s"
          )
        )
      })

      inerital_res_string <- paste(inerital_res_string, collapse = "\n")

      # Remove code in results
      inerital_res_string <- stringr::str_remove_all(
        inerital_res_string,
        pattern = "```[\\w\\W]*```"
      )

      # Remove Figure caption in results
      inerital_res_string <- stringr::str_remove_all(
        inerital_res_string,
        pattern = "\\*\\*Figure[\\w\\W]*\\\\*\\*"
      )

      removeNotification(notice_id)

      withr::with_tempfile("tf", fileext = "md", {
        writeLines(inerital_res_string, "tf.md")
        includeMarkdown(path = "tf.md")
      })
    })

    # > Variables Graph ----
    output$var_factor_map <- renderPlot({
      validate(
        need(input$draw_element,
          message = "Please select the objects you want to plot!"
        ),
        need(input$pc_x != input$pc_y,
          message = "Please select different PCs on x and y axes!"
        )
      )

      visible_elements <- input$draw_element
      invisible_elements <- setdiff(
        c("var", "quanti.sup"),
        visible_elements
      )
      if (length(invisible_elements) == 0) {
        invisible_elements <- "none"
      }


      if (is.null(input$label_element)) {
        label_elements <- "none"
      } else {
        label_elements <- input$label_element
      }

      switch(input$plot_engine,
        "factoextra" = {
          p <- factoextra::fviz_pca_var(
            pca_res(),
            axes = pc_axes(),
            repel = TRUE,
            invisible = invisible_elements,
            label = label_elements,
            col.var = "contrib",
            alpha.var = "cos2",
            cex = 0.5,
            gradient.cols = grDevices::colorRampPalette(c("blue", "red"), alpha = TRUE)(12),
            label.select = focus_vars()$drawn,
          )
        },
        "FactoMineR" = {
          p <- FactoMineR::plot.PCA(
            pca_res(),
            choix = "var",
            invisible = invisible_elements,
            label = label_elements,
            habillage = "contrib",
            select = focus_vars()$drawn,
            cex = 0.9
          )
        }
      )

      p <- p + ggplot2::theme_minimal()

      if (!input$plot_show_legend) {
        p <- p + ggplot2::theme(legend.position = "none")
      } else {
        p <- p + ggplot2::theme(legend.position = "right")
      }

      p <- p + ggplot2::labs(
        title = "Varables factor map",
        subtitle = focus_vars()$what.drawn
      )

      p
    })

    output$var_plot_desc <- renderUI({

      # Load description file for var_factor_map
      desc_file <- app_sys(
        "app/ui/mod_cs_PCA_FactoMineR/var_factor_map_desc.md"
      )
      includeMarkdown(desc_file)
    })

    output$var_bar_plot <- renderPlot({
      switch(input$plot_show_detail,
        "Cos2" = {
          p <- factoextra::fviz_cos2(
            pca_res(),
            choice = "var",
            axes = pc_axes(),
            top = Inf
          )
        },
        "Contribution" = {
          p <- factoextra::fviz_contrib(
            pca_res(),
            choice = "var",
            axes = pc_axes(),
            top = Inf
          )
        }
      )

      p
    })

    output$var_matrix_plot <- renderPlot({
      req(input$pc_x, input$pc_y)

      var <- factoextra::get_pca_var(pca_res())
      switch(input$plot_show_detail,
        "Cos2" = {
          # Find vars of top n cos2
          top_cos2_var <- var$cos2 %>%
            tibble::as_tibble(rownames = "id") %>%
            dplyr::slice_max(
              order_by = .data$Dim.1 + .data$Dim.2 + .data$Dim.3 + .data$Dim.4,
              n = 10
            ) %>%
            dplyr::arrange(
              dplyr::desc(dplyr::across(tidyselect::starts_with("Dim")))
            ) %>%
            tibble::column_to_rownames(var = "id") %>%
            as.matrix()

          p <- corrplot::corrplot(top_cos2_var,
            is.corr = FALSE
          )
        },
        "Contribution" = {
          # Find vars of top n contributions
          top_contrib_var <- var$contrib %>%
            tibble::as_tibble(rownames = "id") %>%
            dplyr::slice_max(
              order_by = .data$Dim.1 + .data$Dim.2 + .data$Dim.3 + .data$Dim.4,
              n = 10
            ) %>%
            dplyr::arrange(
              dplyr::desc(dplyr::across(tidyselect::starts_with("Dim")))
            ) %>%
            tibble::column_to_rownames(var = "id") %>%
            as.matrix()

          p <- corrplot::corrplot(top_contrib_var,
            is.corr = FALSE
            # title = "Top 10 variables of contributions"
          )
        }
      )

      p
    })

    # > Individuals Graph ----
    output$ind_factor_map <- renderPlot({
      validate(
        need(input$draw_element,
          message = "Please select the objects you want to plot!"
        ),
        need(input$pc_x != input$pc_y,
          message = "Please select different PCs on x and y axes!"
        )
      )

      visible_elements <- input$draw_element
      invisible_elements <- setdiff(
        c("ind", "ind.sup", "quali"),
        visible_elements
      )
      if (length(invisible_elements) == 0) {
        invisible_elements <- "none"
      }

      if (is.null(input$label_element)) {
        label_elements <- "none"
      } else {
        label_elements <- input$label_element
      }

      active_inds <- rownames(pca_res()$ind$coord)
      raw_data <- pca_res()$call$X
      quali_var <- raw_data[active_inds, input$ind_color_point_quali_var]

      switch(input$ind_color_point,
        "qualitative variable" = {
          if (!is.null(quali_var)) {
            habillage_points <- input$ind_color_point_quali_var
            addEllipses <- input$ind_color_point_quali_ellipses
          } else {
            habillage_points <- "none"
            addEllipses <- FALSE
          }
        },
        "active/supplementary" = {
          habillage_points <- "none"
        }
      )

      if (input$ind_color_point == "active/supplementary") {
        addEllipses <- FALSE
      }

      group_category <- sort(unique(quali_var))
      group_color_mapping <- scales::col_factor(
        palette = scales::brewer_pal(type = "qual", palette = "Paired")(12),
        domain = group_category
      )
      group_pal <- group_color_mapping(group_category)
      if (length(group_pal) == 0) {
        group_pal <- NULL
      }

      switch(input$plot_engine,
        "factoextra" = {
          # Plot individuals by factoextra::fviz_pca_ind

          # Use contrib for size, Cos2 for alpha

          # When there are supplementary individuals, it's impossible to
          # plot supplementary individuals in size of contribution,
          # because they don't have contribution.
          if (is.null(pca_res()$ind.sup)) {
            size_points <- "contrib"
          } else {
            size_points <- 1.5
          }

          p <- factoextra::fviz_pca_ind(
            pca_res(),
            axes = pc_axes(),
            repel = FALSE,
            invisible = invisible_elements,
            label = label_elements,
            label.select = focus_inds()$drawn,
            labelsize = 3,
            habillage = habillage_points,
            col.ind = input$ind_color_point_active_individual,
            alpha.ind = "cos2",
            col.ind.sup = input$ind_color_point_sup_individual,
            col.quali = input$ind_color_point_quali_category,
            pointshape = 19,
            pointsize = size_points,
            addEllipses = addEllipses,
            ellipse.type = "confidence",
            palette = group_pal,
            legend.title = list(
              color = habillage_points,
              fill = habillage_points
            )
          )

          # Add point and label categories of supplementary quality variables
          if (("quali" %in% visible_elements) && !is.null(pca_res()$quali.sup)) {

            # Whether to add label to supplementary categories
            if ("quali" %in% label_elements) {
              label_qual <- TRUE
            } else {
              label_qual <- FALSE
            }

            # Color to draw category
            if (input$ind_color_point == "active/supplementary") {
              category_color <- input$ind_color_point_quali_category
            } else {
              category_color <- group_color_mapping(
                rownames(pca_res()$quali.sup$coord)
              )
            }

            p <- factoextra::fviz_add(p,
              pca_res()$quali.sup$coord,
              axes = pc_axes(),
              addlabel = label_qual,
              shape = 22,
              pointsize = 2,
              labelsize = 3,
              color = category_color
            )

            # Resize viewpoint to display to show quality categories clearly,
            # if only supplementary quality variables is visible in plane
            if ("ind" %in% invisible_elements) {
              if (("ind.sup" %in% invisible_elements) ||
                is.null(pca_res()$ind.sup)) {
                pc_x <- pc_axes()[1]
                pc_y <- pc_axes()[2]
                xlims <- range(pca_res()$quali.sup$coord[, glue::glue("Dim.{pc_x}")]) * 1.5
                ylims <- range(pca_res()$quali.sup$coord[, glue::glue("Dim.{pc_y}")]) * 1.5

                p <- p + ggplot2::xlim(xlims) + ggplot2::ylim(ylims)
              }
            }
          }
        },
        "FactoMineR" = {
          # Plot individuals by FactoMineR::plot.PCA/plotellipses
          if (!addEllipses) {
            p <- FactoMineR::plot.PCA(
              pca_res(),
              axes = pc_axes(),
              choix = "ind",
              invisible = invisible_elements,
              label = label_elements,
              select = focus_inds()$drawn,
              habillage = habillage_points,
              palette = group_pal,
              col.ind = input$ind_color_point_active_individual,
              col.ind.sup = input$ind_color_point_sup_individual,
              col.quali = input$ind_color_point_quali_category,
              ggoptions = list(size = 3)
            )
          } else {
            p <- FactoMineR::plotellipses(
              pca_res(),
              axes = pc_axes(),
              invisible = invisible_elements,
              label = label_elements,
              select = focus_inds()$drawn,
              habillage = habillage_points,
              palette = group_pal,
              col.ind = input$ind_color_point_active_individual,
              col.ind.sup = input$ind_color_point_sup_individual,
              col.quali = input$ind_color_point_quali_category,
              ggoptions = list(size = 3)
            )
          }
        },
      )

      p <- p + ggplot2::theme_minimal()

      if (!input$plot_show_legend) {
        p <- p + ggplot2::theme(legend.position = "none")
      } else {
        p <- p + ggplot2::theme(legend.position = "right")
      }

      p <- p + ggplot2::labs(
        title = "Individuals factor map",
        subtitle = focus_inds()$what.drawn
      )

      p
    })

    output$ind_plot_desc <- renderUI({

      # Load description file for var_factor_map
      desc_file <- app_sys(
        "app/ui/mod_cs_PCA_FactoMineR/ind_factor_map_desc.md"
      )
      includeMarkdown(desc_file)
    })

    output$ind_bar_plot <- renderPlot({
      switch(input$plot_show_detail,
        "Cos2" = {
          p <- factoextra::fviz_cos2(
            pca_res(),
            choice = "ind",
            axes = pc_axes(),
            top = 50
          )
        },
        "Contribution" = {
          p <- factoextra::fviz_contrib(
            pca_res(),
            choice = "ind",
            axes = pc_axes(),
            top = 50
          )
        }
      )

      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))

      p
    })

    output$ind_matrix_plot <- renderPlot({
      req(input$pc_x, input$pc_y)

      ind <- factoextra::get_pca_ind(pca_res())
      switch(input$plot_show_detail,
        "Cos2" = {
          # Find inds of top n cos2
          top_cos2_ind <- ind$cos2 %>%
            tibble::as_tibble(rownames = "id") %>%
            dplyr::slice_max(
              order_by = .data$Dim.1 + .data$Dim.2 + .data$Dim.3 + .data$Dim.4,
              n = 10
            ) %>%
            dplyr::arrange(
              dplyr::desc(dplyr::across(tidyselect::starts_with("Dim")))
            ) %>%
            tibble::column_to_rownames(var = "id") %>%
            as.matrix()

          p <- corrplot::corrplot(top_cos2_ind,
            is.corr = FALSE
          )
        },
        "Contribution" = {
          # Find vars of top n contributions
          top_contrib_ind <- ind$contrib %>%
            tibble::as_tibble(rownames = "id") %>%
            dplyr::slice_max(
              order_by = .data$Dim.1 + .data$Dim.2 + .data$Dim.3 + .data$Dim.4,
              n = 10
              ) %>%
            dplyr::arrange(
              dplyr::desc(dplyr::across(tidyselect::starts_with("Dim")))
            ) %>%
            tibble::column_to_rownames(var = "id") %>%
            as.matrix()

          p <- corrplot::corrplot(top_contrib_ind,
            is.corr = FALSE
            # title = "Top 10 individuals of contributions"
          )
        }
      )

      p
    })

    # > Plane description ----
    output$plane_joint_factor_map <- renderPlot({
      validate(
        need(input$draw_element,
          message = "Please select the objects you want to plot: Individuals, Supplementary individuals, Categories!"
        ),
        need(input$pc_x != input$pc_y,
          message = "Please select different PCs on x and y axes!"
        )
      )

      visible_elements <- input$draw_element
      invisible_elements <- setdiff(
        c("ind", "ind.sup", "quali", "var", "quanti.sup"),
        visible_elements
      )
      if (length(invisible_elements) == 0) {
        invisible_elements <- "none"
      }

      if (is.null(input$label_element)) {
        label_elements <- "none"
      } else {
        label_elements <- input$label_element
      }

      active_inds <- rownames(pca_res()$ind$coord)
      raw_data <- pca_res()$call$X
      quali_var <- raw_data[active_inds, input$ind_color_point_quali_var]

      switch(input$ind_color_point,
        "qualitative variable" = {
          if (!is.null(quali_var)) {
            fill_points <- quali_var
            addEllipses <- input$ind_color_point_quali_ellipses
          } else {
            fill_points <- "darkgreen"
            addEllipses <- FALSE
          }
        },
        "active/supplementary" = {
          fill_points <- input$ind_color_point_active_individual
        }
      )

      if (input$ind_color_point == "active/supplementary") {
        addEllipses <- FALSE
      }

      group_category <- sort(unique(quali_var))
      group_color_mapping <- scales::col_factor(
        palette = scales::brewer_pal(type = "qual", palette = "Paired")(12),
        domain = group_category
      )
      group_pal <- group_color_mapping(group_category)
      if (length(group_pal) == 0) {
        group_pal <- NULL
      }

      # When there are supplementary individuals, it's impossible to
      # plot supplementary individuals in size of contribution,
      # because they don't have contribution.
      if (is.null(pca_res()$ind.sup)) {
        size_points <- "contrib"
      } else {
        size_points <- 1.5
      }

      # Set label for points

      # Label points for selected individuals and variables
      if (!is.null(focus_vars()$drawn) && !is.null(focus_inds()$drawn)) {
        label_select <- c(focus_inds()$drawn, focus_vars()$drawn)
      }
      # Label points for all individuals and variables
      if (is.null(focus_vars()$drawn) && is.null(focus_inds()$drawn)) {
        label_select <- NULL
      }

      # Label selected individuals and all variables
      if (is.null(focus_vars()$drawn) && !is.null(focus_inds()$drawn)) {
        label_select <- c(focus_inds()$drawn, rownames(pca_res()$var$coor))
      }

      # Label selected variables and all individuals
      if (is.null(focus_inds()$drawn) && !is.null(focus_vars()$drawn)) {
        label_select <- c(focus_vars()$drawn, rownames(pca_res()$ind$coor))
      }

      # Plot biplot by factoextra::fviz_pca_biplot
      p <- factoextra::fviz_pca_biplot(

        pca_res(),
        axes = pc_axes(),

        # Setting for individuals
        habillage = "none",
        col.ind = "black",
        fill.ind = fill_points,
        alpha.ind = "cos2",
        col.ind.sup = input$ind_color_point_sup_individual,
        col.quali = input$ind_color_point_quali_category,
        pointshape = 21,
        pointsize = size_points,
        addEllipses = addEllipses,
        ellipse.type = "confidence",
        palette = group_pal,

        # Setting for vars
        col.var = "contrib",
        alpha.var = "cos2",
        gradient.cols = grDevices::colorRampPalette(c("blue", "red"), alpha = TRUE)(12),

        # Common setting
        repel = FALSE,
        invisible = invisible_elements,
        label = label_elements,
        labelsize = 3,
        label.select = label_select,
        legend.title = list(
          fill = input$ind_color_point_quali_var,
          color = "Contrib"
        )
      )

      # Add point and label categories of supplementary quality variables
      if (("quali" %in% visible_elements) && !is.null(pca_res()$quali.sup)) {

        # Whether to add label to supplementary categories
        if ("quali" %in% label_elements) {
          label_qual <- TRUE
        } else {
          label_qual <- FALSE
        }

        # Color to draw category
        if (input$ind_color_point == "active/supplementary") {
          category_color <- input$ind_color_point_quali_category
        } else {
          category_color <- group_color_mapping(
            rownames(pca_res()$quali.sup$coord)
          )
        }

        p <- factoextra::fviz_add(p,
          pca_res()$quali.sup$coord,
          axes = pc_axes(),
          addlabel = label_qual,
          shape = 22,
          pointsize = 2,
          labelsize = 3,
          color = category_color
        )

        # Resize viewpoint to display to show quality categories clearly,
        # if only supplementary quality variables is visible in plane
        if ("ind" %in% invisible_elements) {
          if (("ind.sup" %in% invisible_elements) ||
            is.null(pca_res()$ind.sup)) {
            pc_x <- pc_axes()[1]
            pc_y <- pc_axes()[2]
            xlims <- range(pca_res()$quali.sup$coord[, glue::glue("Dim.{pc_x}")]) * 1.5
            ylims <- range(pca_res()$quali.sup$coord[, glue::glue("Dim.{pc_y}")]) * 1.5

            p <- p + ggplot2::xlim(xlims) + ggplot2::ylim(ylims)
          }
        }

        p <- p + ggplot2::theme_minimal()
      }

      if (!input$plot_show_legend) {
        p <- p + ggplot2::theme(legend.position = "none")
      } else {
        p <- p + ggplot2::theme(legend.position = "right")
      }

      p <- p + ggplot2::labs(
        title = "Biplot map(PCA)",
        subtitle = glue::glue("{focus_vars()$what.drawn}\n{focus_inds()$what.drawn}")
      )

      p
    })

    output$plane_joint_factor_desc <- renderUI({
      dim_desc_res_string <- utils::capture.output({
        invisible(
          FactoInvestigate::description(pca_res(), dim = pc_axes())
        )
      })

      dim_desc_res_string <- paste(dim_desc_res_string, collapse = "\n")

      # Remove code in results
      dim_desc_res_string <- stringr::str_remove_all(
        dim_desc_res_string,
        pattern = "```[\\w\\W]*```"
      )

      # Remove Figure caption in results
      dim_desc_res_string <- stringr::str_remove_all(
        dim_desc_res_string,
        pattern = "\\*\\*Figure[\\w\\W]*\\\\*\\*"
      )

      withr::with_tempfile("tf", fileext = "md", {
        writeLines(dim_desc_res_string, "tf.md")
        includeMarkdown(path = "tf.md")
      })
    })

    # > Classification HCPC ----

    output$hcpc_cluster_plot <- renderPlot({
      switch(input$plot_engine,
        "factoextra" = {
          notice_id <- showNotification(
            "Plot HCPC result, it might take a while...",
            duration = NULL
          )

          switch(input$"classfication_hcpc_plot_factoextra",
            "dendrogram" = {
              show_label <- input$classfication_hcpc_show_label

              # Abort dendrograms for too much cases.
              if (input$classfication_hcpc_plot_factoextra_dendrogram_type
                %in% c("circular")) {
                data_length <- nrow(df_pca())
                msg <- glue::glue(
                  "Too many data({data_length} cases) would cause cashing on circular dengrograms.
                 Please use fewer data."
                )
                validate(
                  need(data_length < 300, message = msg)
                )
              }

              p <- factoextra::fviz_dend(
                hcpc_res(),
                repel = FALSE,
                type = input$classfication_hcpc_plot_factoextra_dendrogram_type,
                cex = 0.7, # Label size
                horiz = TRUE,
                rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                rect_border = "aaas", # Rectangle color
                show_labels = show_label,
                labels_track_height = 0.8, # Augment the room for labels
                palette = "aaas", # Color palette see ?ggpubr::ggpar
                main = "Dengrogram of individuals clusters by HCPC"
              )
            },
            "cluster" = {
              if (input$classfication_hcpc_show_label) {
                label.select <- rep_inds_hcpc()
                geom <- c("point", "text")
              } else {
                geom <- c("point")
                label.select <- NULL
              }
              p <- factoextra::fviz_cluster(
                hcpc_res(),
                axes = pc_axes(),
                repel = FALSE, # Avoid label overlapping
                geom = geom,
                label.select = label.select,
                labelsize = 10,
                palette = "aaas", # Color palette see ?ggpubr::ggpar
                ggtheme = ggplot2::theme_minimal(),
                main = "Map of individuals clusters by HCPC"
              )
            }
          )

          removeNotification(notice_id)
        },
        "FactoMineR" = {
          show_label <- input$classfication_hcpc_show_label

          choice <- input$classfication_hcpc_plot_FactoMineR
          switch(choice,
            "tree" = {
              p <- FactoMineR::plot.HCPC(
                hcpc_res(),
                axes = pc_axes(),
                choice = "tree"
              )
            },
            "bar" = {
              p <- FactoMineR::plot.HCPC(
                hcpc_res(),
                axes = pc_axes(),
                choice = "bar"
              )
            },
            "map" = {
              p <- FactoMineR::plot.HCPC(
                hcpc_res(),
                axes = pc_axes(),
                choice = "map",
                draw.tree = TRUE,
                ind.names = show_label,
                select = rep_inds_hcpc()
              )
            },
            "3D.map" = {
              p <- FactoMineR::plot.HCPC(
                hcpc_res(),
                axes = pc_axes(),
                choice = "3D.map",
                ind.names = show_label,
                centers.plot = TRUE
              )
            }
          )
        }
      )

      p <- p + ggplot2::theme_minimal()

      p
    })
    output$hcpc_cluster_desc_overall <- renderUI({
      notice_id <- showNotification(
        "Generate Cluster description, it might take a while...",
        duration = NULL
      )

      class_res_string <- utils::capture.output({
        invisible(
          class_res <- FactoInvestigate::classif(pca_res(), graph = FALSE)
        )
      })

      removeNotification(notice_id)

      class_res_string <- paste(class_res_string, collapse = "\n")

      # Remove code in results
      class_res_string <- stringr::str_remove_all(
        class_res_string,
        pattern = "```[\\w\\W]*```"
      )

      # Remove Figure caption in results
      class_res_string <- stringr::str_remove_all(
        class_res_string,
        pattern = "\\*\\*Figure[\\w\\W]*\\.\\*\\*"
      )

      withr::with_tempfile("tf", fileext = "md", {
        writeLines(class_res_string, "tf.md")
        includeMarkdown(path = "tf.md")
      })
    })

    output$hcpc_cluster_desc_by_var <- renderPrint({
      hcpc_res()$desc.var
    })

    output$hcpc_cluster_desc_by_dim <- renderPrint({
      hcpc_res()$desc.axes
    })

    output$hcpc_cluster_desc_by_ind <- renderPrint({
      hcpc_res()$desc.ind
    })

    output$hcpc_cluster_table_result <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        cluster_result_hcpc(),
        expect_type = "numeric"
      )

      DT::datatable(
        cluster_result_hcpc(),
        filter = "top",
        extensions = "Scroller",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          ),
          pageLength = 5,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 180,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = numeric_vars, digits = 2)
    })

    output$hcpc_cluster_table_rep_ind <- DT::renderDataTable({
      data_clust <- hcpc_res()$data.clust %>%
        tibble::as_tibble(rownames = "stkcd")

      df_clust_rep_inds <- data_clust %>%
        dplyr::filter(.data$stkcd %in% rep_inds_hcpc()) %>%
        dplyr::arrange(.data$clust) %>%
        dplyr::mutate(
          indname = code2name(as.character(.data$indcd)),
          stkname = code2name(.data$stkcd)
        ) %>%
        dplyr::rename(cluster = .data$clust) %>%
        dplyr::select(
          c("cluster", "stkcd", "stkname", "indcd", "indname"),
          tidyselect::everything()
        )

      numeric_vars <- zstmodelr::expect_type_fields(
        df_clust_rep_inds,
        expect_type = "numeric"
      )

      DT::datatable(df_clust_rep_inds,
        filter = "top",
        extensions = "Scroller",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          ),
          pageLength = 10,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 180,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = numeric_vars, digits = 2)
    })

    output$hcpc_cluster_table_mapping <- DT::renderDataTable({
      cluster_mapping_hcpc <-
        cluster_result_hcpc() %>%
        dplyr::nest_by(.data$cluster) %>%
        dplyr::mutate(
          indcds = list(sort(unique(.data$data$indcd))),
          indnames = list(sort(unique(.data$data$indname))),
          stkcds = list(sort(unique(.data$data$stkcd))),
          stknames = list(sort(unique(.data$data$stkname)))
        ) %>%
        dplyr::select(-c("data", "indcds", "stkcds"))

      DT::datatable(cluster_mapping_hcpc,
        filter = "top",
        extensions = "Scroller",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          ),
          pageLength = 10,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 180,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })
  })
}

#' Testing module app of cs_PCA_FactoMineR
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_PCA_FactoMineR  Testing App of cs_PCA_FactoMineR.
cs_PCA_FactoMineR_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  # Filter subset for test
  indcd_group <- c("C38", "C39")
  cs_date <- "2018-12-31"

  csbl_vars <- csbl_vars %>%
    dplyr::filter(.data$indcd %in% indcd_group) %>%
    dplyr::filter(grepl(cs_date, .data$id))


  ui <- fluidPage(
    cs_PCA_FactoMineR_ui("cs_PCA_FactoMineR_module")
  )
  server <- function(input, output, session) {
    cs_PCA_FactoMineR_server(
      "cs_PCA_FactoMineR_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
