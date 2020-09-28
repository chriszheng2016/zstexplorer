#' cs_cluster_factoextra
#'
#' @description A shiny module for cs_cluster_factoextra.
#'
#' @details
#'  The module is an UI for user to display plots of ...
#'  by [`factoextra`][factoextra::factoextra] package.
#'
#' @name cs_cluster_factoextra
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cluster_factoextra_ui("cs_cluster_factoextra_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cluster_factoextra <- cs_cluster_factoextra_server(
#'     "cs_cluster_factoextra_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_cluster_factoextra_app()
#' }
#'
NULL

#' UI function of cs_cluster_factoextra
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cluster_factoextra  UI function of cs_cluster_factoextra.
#' @importFrom shiny NS tagList
cs_cluster_factoextra_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = ns("dist_metric"),
          label = strong("Distance Metric:"),
          choices = c(
            "euclidean", "manhattan", "pearson",
            "spearman", "kendall"
          ),
          selected = "euclidean"
        ),

        tabsetPanel(
          id = ns("plot_setting"),
          type = "hidden",
          tabPanelBody(
            value = "trendency_plot",
          ),
          tabPanelBody(
            value = "nbclust_plot",
            selectInput(
              inputId = ns("nbclust_method"),
              label = strong("NbClust Method:"),
              choices = c(
                "kmeans", "ward.D", "ward.D2", "single",
                "complete", "average", "median", "centroid"
              )
            )
          ),
          tabPanelBody(
            value = "clustering_plot",
            selectInput(
              inputId = ns("clust_type"),
              label = strong("Clustering Type:"),
              choices = c(
                "Partitioning Clustering",
                "Hierarchical Clustering"
              )
            ),
            selectInput(
              inputId = ns("clust_algorithm"),
              label = strong("Clustering Algorithm:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("fviz_nbclust_method"),
              label = strong("fviz_nbclust method:"),
              choices = c("silhouette", "wss", "gap_stat")
            ),
            sliderInput(
              inputId = ns("cluster_k"),
              label = strong("Number of Clusters(k) :"),
              min = 2,
              max = 10,
              value = 2,
              step = 1
            ),
            selectInput(
              inputId = ns("hc_linkage_method"),
              label = strong("HC method:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("hc_dend_type"),
              label = strong("HC dendrogram:"),
              choices = c("rectangle", "circular", "phylogenic")
            )
          ),
          tabPanelBody(
            value = "validation_plot",

            selectInput(
              inputId = ns("validation_clmethod"),
              label = strong("Cluster Methods:"),
              multiple = TRUE,
              choices = c(
                "kmeans", "pam", "clara",
                "hierarchical", "agnes", "diana"
              ),
              selected = c(
                "kmeans", "pam", "clara",
                "hierarchical", "agnes", "diana"
              )
            ),

            selectInput(
              inputId = ns("validation_dist_metric"),
              label = strong("Distance Metric:"),
              choices = c("euclidean", "correlation", "manhattan"),
              selected = "euclidean"
            ),

            selectInput(
              inputId = ns("validation_method"),
              label = strong("Hc Method:"),
              choices = c("ward", "single", "complete", "average"),
              selected = "average"
            ),

            radioButtons(
              inputId = ns("validation_measure"),
              label = strong("Validation Measure"),
              choices = list(
                "Internal Measure" = "internal",
                "Stability Measure" = "stability"
              ),
              selected = "internal"
            ),

            conditionalPanel(
              condition = "input.validation_measure == 'internal'",
              selectInput(
                inputId = ns("internal_measure"),
                label = strong("Internal Measure:"),
                choices = c("Silhouette", "Dunn", "Connectivity")
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "input.validation_measure == 'stability'",

              selectInput(
                inputId = ns("stability_measure"),
                label = strong("Stability Measure:"),
                choices = c("APN", "AD", "ADM", "FOM")
              ),
              ns = ns
            ),
          ),
          tabPanelBody(
            value = "pvhc_plot",

            selectInput(
              inputId = ns("pvhc_dist_metric"),
              label = strong("Distance Metric:"),
              choices = c(
                "correlation", "uncentered", "abscor",
                "euclidean", "maximum", "manhattan",
                "canberra", "binary", "minkowski"
              ),
              selected = "correlation"
            ),

            selectInput(
              inputId = ns("pvhc_method"),
              label = strong("HC Method:"),
              choices = c(
                "ward.D", "ward.D2", "single", "complete", "average",
                "mcquitty", "median", "centroid"
              ),
              selected = "average"
            ),
            sliderInput(
              inputId = ns("pvhc_alpha"),
              label = strong("Find Clusters above P-value:"),
              min = 0.0,
              max = 1,
              value = 0.95,
              step = 0.05
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
            "Cluster Trendency",
            box(
              title = "Hopkins statistic", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              width = 12,
              uiOutput(ns("hopkins_stats_description")),
              verbatimTextOutput(ns("cluster_trendency_stats"))
            ),
            box(
              title = "Distance Map", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              width = 12,

              plotOutput(ns("cluster_trendency_plot"))
            )
          ),
          tabPanel(
            "Best Number of clusters",
            box(
              title = "NbClust::NbClust() Plot", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, width = 12,
              plotOutput(ns("nbclust_indices_plot"))
            )
          ),
          tabPanel(
            "Clustering",
            fluidRow(
              box(
                title = "Find Optimal k", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 6,

                # plotly::plotlyOutput(ns("optimal_k_plot"))
                plotOutput(ns("optimal_k_plot"))
              ),
              box(
                title = "Cluster Plots", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 6,

                # plotly::plotlyOutput(ns("clusters_plot"))
                plotOutput(ns("clusters_plot"))
              )
            ),

            box(
              title = "Cluster Mapping", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              width = 12,
              tabsetPanel(
                id = ns("clusters_mapping"),
                type = "pills",

                tabPanel(
                  "Mapping to Clusters",
                  DT::dataTableOutput(ns("mapping_to_clusters_table"))
                ),
                tabPanel(
                  "Mapping from Clusters",
                  DT::dataTableOutput(ns("mapping_from_clusters_table"))
                )
              )
            ),
          ),
          tabPanel(
            "Best algorithm",
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Validation Measures", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, width = 12,

                  uiOutput(ns("validation_meausre_description"))
                )
              ),
              column(
                width = 6,
                box(
                  title = "Measure Summary", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, width = 12,

                  tableOutput(ns("validation_summary_table"))
                ),
                box(
                  title = "Measure Plot", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, width = 12,

                  plotOutput(ns("validation_meausre_plot"))
                )
              )
            )
          ),
          tabPanel(
            "P-value for HC",
            box(
              title = "Compute P-value for Hierarchical Clustering",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              width = 12,

              uiOutput(ns("pvalue_hc_description"))
            ),

            tabBox(
              id = ns("pvalue_hc_pick_tabset"),
              width = 12,

              tabPanel(
                "Picked Clusters Plot",
                plotOutput(ns("pvalue_hc_pick_plot"))
              ),
              tabPanel(
                "Picked Clusters Mapping",
                DT::dataTableOutput(ns("pvalue_hc_pick_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' Server function of cs_cluster_factoextra
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_cluster_factoextra  Server function of cs_cluster_factoextra.
#' @return * Server function return a data frame of ...
cs_cluster_factoextra_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Logic reactive ----

    df_cluster_raw <- reactive({
      csbl_vars() %>%
        na.omit() %>%
        tidyr::separate(.data$id,
          into = c("date", "period", "stkcd"), sep = "_"
        ) %>%
        dplyr::select(-c("date", "period")) %>%
        # Use median for numeric field of multiple period series of stock
        # Use mode value for non-numeric filed of multiple period series of stock
        dplyr::group_by(.data$stkcd) %>%
        dplyr::summarise(
          indcd = zstmodelr::mode_value(.data$indcd),
          dplyr::across(where(is.numeric), median)
        ) %>%
        # Scale data
        dplyr::mutate(dplyr::across(where(is.numeric), scale)) %>%
        tibble::column_to_rownames(var = "stkcd")
    })

    # Dataset for clustering
    df_cluster <- reactive({
      df_cluster_raw() %>%
        dplyr::select(where(is.numeric))
    })

    # Dataset for mapping industry code
    df_cluster_indcd <- reactive({
      df_cluster_raw() %>%
        dplyr::select(c("indcd"))
    })

    # Distance matrix for clustering
    dist <- reactive({

      notice_id <- showNotification(
        "Compute distance matrix, it might take a while...",
        duration = NULL
      )

      dist <- factoextra::get_dist(
        x = df_cluster(),
        method = req(input$dist_metric)
      )

      removeNotification(notice_id)

      dist
    })

    # hopkins stats for cluster trendency
    hopkins_stats <- reactive({
      notice_id <- showNotification(
        "Compute hopkins stats, it might take a while...",
        duration = NULL
      )

      cluster_trendency <- factoextra::get_clust_tendency(
        df_cluster(),
        n = min(10, nrow(df_cluster()) - 1),
        graph = FALSE,
        seed = 123
      )

      hopkins_res <- cluster_trendency$hopkins_stat

      removeNotification(notice_id)

      hopkins_res
    })

    # Use NbCluster() to determine best number of clusters
    nbclust_res <- reactive({
      notice_id <- showNotification(
        "Compute bnc by NbCluster(), it might take a while...",
        duration = NULL
      )

      nbclust_res <- NbClust::NbClust(
        data = df_cluster(),
        diss = dist(),
        distance = NULL, method = req(input$nbclust_method)
      )

      removeNotification(notice_id)

      nbclust_res
    })


    # Compute Clustering
    cluster_res <- reactive({
      notice_id <- showNotification(
        "Compute partition clustering, it might take a while...",
        duration = NULL
      )

      result <- NULL
      switch(req(input$clust_algorithm),
        "K-Means:kmeans()" = {
          result <- kmeans(df_cluster(),
            centers = req(input$cluster_k),
            nstart = 25
          )
          cluster <- result$cluster
        },
        "K-Medoids:pam()" = {
          result <- cluster::pam(dist(),
            k = req(input$cluster_k)
          )
          cluster <- result$clustering
        },
        "Large Application:clara()" = {
          clara_metric <- c("euclidean", "manhattan", "jaccard")
          result <- if (req(input$dist_metric) %in% clara_metric) {
            cluster::clara(df_cluster(),
              k = req(input$cluster_k),
              metric = req(input$dist_metric)
            )
          } else {
            cluster::clara(df_cluster(),
              k = req(input$cluster_k),
              metric = "euclidean"
            )
          }
          cluster <- result$clustering
        },
        "Basic HC:hclust()" = {
          result <- factoextra::hcut(dist(),
            k = req(input$cluster_k),
            hc_func = "hclust",
            hc_method = req(input$hc_linkage_method)
          )
          cluster <- result$cluster
        },
        "Agglomerative HC:agnes()" = {
          result <- factoextra::hcut(dist(),
            k = req(input$cluster_k),
            hc_func = "agnes",
            hc_method = req(input$hc_linkage_method)
          )
          cluster <- result$cluster
        },
        "Divisive HC:diana()" = {
          result <- factoextra::hcut(dist(),
            k = req(input$cluster_k),
            hc_func = "diana",
            hc_method = req(input$hc_linkage_method)
          )
          cluster <- result$cluster
        },
        {
          result <- NULL
        }
      )

      removeNotification(notice_id)

      cluster_res <- list(
        # raw result of various clustering functions
        res = result,
        # cluster and data field are general interface for fviz_cluster() to use
        cluster = cluster,
        data = df_cluster()
      )

      cluster_res
    })

    # Compute Clusters mapping to industry and stock
    cluster_mapping <- reactive({
      cluster_res <- cluster_res()

      ds_cluster_res <- cbind(
        cluster = cluster_res$cluster,
        df_cluster_indcd(),
        cluster_res$data
      )

      cluster_mapping <- ds_cluster_res %>%
        dplyr::select(indcd, cluster) %>%
        tibble::rownames_to_column(var = "stkcd") %>%
        dplyr::mutate(
          indname = zstexplorer:::code2name(indcd),
          stkname = zstexplorer:::code2name(stkcd)
        ) %>%
        dplyr::select(stkcd, stkname, indcd, indname, cluster)

      cluster_mapping
    })

    # Use fviz_nbclust to visualize optimal number of clusters
    fviz_nbclust_plot <- reactive({
      fviz_nbclust_method <- req(input$fviz_nbclust_method)
      nbclust_plot <- switch(req(input$clust_algorithm),
        "K-Means:kmeans()" = {
          factoextra::fviz_nbclust(
            df_cluster(),
            FUNcluster = kmeans,
            method = fviz_nbclust_method
          )
        },
        "K-Medoids:pam()" = {
          factoextra::fviz_nbclust(
            df_cluster(),
            FUNcluster = cluster::pam,
            method = fviz_nbclust_method
          )
        },
        "Large Application:clara()" = {
          factoextra::fviz_nbclust(
            df_cluster(),
            FUNcluster = cluster::clara,
            method = fviz_nbclust_method
          )
        },
        NULL
      )

      nbclust_plot <- nbclust_plot +
        geom_vline(xintercept = req(input$cluster_k), linetype = 2, color = "red")

      nbclust_plot
    })

    # Use fviz_dend to visualize hierarchical cluster
    fviz_dend_plot <- reactive({

      # Abort dendrograms for too much cases.
      data_length <- nrow(df_cluster())
      if (req(input$hc_dend_type) %in% c("circular")
      & data_length > 300) {
        msg <- glue::glue("Too many data({data_length} cases) would cause cashing on circular dengrograms.
                          Please use fewer data.")
        rlang::abort(msg)
      }

      # Plot dendrogram for hc results
      hc_res <- cluster_res()$res
      if (!is.null(hc_res) & inherits(hc_res, "hcut")) {
        notice_id <- showNotification(
          "Plot dendrogram, it might take a while...",
          duration = NULL
        )

        dend_plot <- factoextra::fviz_dend(
          hc_res,
          rect = TRUE, horiz = TRUE,
          type = req(input$hc_dend_type)
        )

        removeNotification(notice_id)

        dend_plot
      }
    })

    # Compute cluster validation by clValid::clValid()
    clValid_res <- reactive({
      result <- clValid::clValid(
        as.matrix(df_cluster()),
        nClust = 2:6,
        clMethods = req(input$validation_clmethod),
        metric = req(input$validation_dist_metric),
        method = req(input$validation_method),
        validation = req(input$validation_measure)
      )
    })

    # Compute pvalue for hierarchical clustering
    pvhc_res <- reactive({
      notice_id <- showNotification(
        "Compute P-value for HC, it might take a while...",
        duration = NULL
      )

      set.seed(123)
      pvhc_res <- pvclust::pvclust(
        t(df_cluster()),
        method.dist = req(input$pvhc_dist_metric),
        method.hclust = req(input$pvhc_method),
        nboot = 10
      )

      removeNotification(notice_id)

      pvhc_res
    })


    # User control interaction ----

    # Update setting UI when user choose plot tabs
    observeEvent(input$plot_tabs, ignoreInit = TRUE, {

      # Update setting_tabs according to plot type
      switch(req(input$plot_tabs),
        "Cluster Trendency" = {
          shinyjs::show(id = "dist_metric")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "trendency_plot"
          )
        },
        "Best Number of clusters" = {
          shinyjs::show(id = "dist_metric")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "nbclust_plot"
          )
        },
        "Clustering" = {
          shinyjs::show(id = "dist_metric")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "clustering_plot"
          )
        },
        "Best algorithm" = {

          # Hide dist_metric to use its own distance metric control
          # (validation_dist_metric)
          shinyjs::hide(id = "dist_metric")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "validation_plot"
          )
        },
        "P-value for HC" = {

          # Hide dist_metric to use its own distance metric control
          # (pvhc_dist_metric)
          shinyjs::hide(id = "dist_metric")

          updateTabsetPanel(session,
            inputId = "plot_setting",
            selected = "pvhc_plot"
          )
        },
      )
    })

    # Update UI with dataset and user inputs
    observe({

      # Set choices for select inputs

      # # Set choices for clust_algorithm basing on cluster_type
      pc_algs <- c(
        "K-Means:kmeans()",
        "K-Medoids:pam()",
        "Large Application:clara()"
      )
      hc_algs <- c(
        "Basic HC:hclust()",
        "Agglomerative HC:agnes()",
        "Divisive HC:diana()"
      )

      if (req(input$clust_type) == "Partitioning Clustering") {
        updateSelectInput(
          session = session, inputId = "clust_algorithm",
          choices = pc_algs
        )
      } else {
        updateSelectInput(
          session = session, inputId = "clust_algorithm",
          choices = hc_algs
        )
      }


      # Set choices for hc_method
      hc_method <- c(
        "ward.D", "single", "complete",
        "average", "mcquitty", "median", "centroid",
        "ward.D2"
      )
      updateSelectInput(
        session = session, inputId = "hc_linkage_method",
        choices = hc_method
      )
    })


    # Render output ----

    # >> Cluster Tendency output ----

    output$hopkins_stats_description <- renderUI({

      # Load description file for hopkins_stats
      desc_file <- app_sys(
        "app/ui/mod_cs_cluster_factoextra/hopkins_stats.md"
      )
      includeMarkdown(desc_file)
    })


    output$cluster_trendency_stats <- renderPrint({
      hopkins_stats <- hopkins_stats()

      result_msg <- glue::glue(
        "Hopkins statistic: {format(hopkins_stats, digits = 3)}"
      )

      if ((1 - hopkins_stats) <= 0.10) {
        result_msg <- glue::glue("{result_msg}, near to one which means a significantly a clusterable data")
      }

      if (abs(hopkins_stats - 0.5) <= 0.10) {
        result_msg <- glue::glue("{result_msg}, near to 0.5 which means a uniform distributed data")
      }

      cat(
        result_msg
      )
    })

    output$cluster_trendency_plot <- renderPlot({
      metric <- req(input$dist_metric)
      factoextra::fviz_dist(dist(),
        order = TRUE,
        show_labels = FALSE,
        lab_size = 6
      ) + labs(
        # title = "Distance Matrix",
        subtitle = glue::glue("metric:{metric}")
      )
    })

    # >> Best Number of cluster output ----
    output$nbclust_indices_plot <- renderPlot({
      metric <- req(input$dist_metric)
      method <- req(input$nbclust_method)
      factoextra::fviz_nbclust(nbclust_res()) +
         labs(subtitle = glue::glue("metric:{metric}, method:{method}"))
    })

    # >> Clustering output ----
    # output$optimal_k_plot <- plotly::renderPlotly({
    output$optimal_k_plot <- renderPlot({
      if (req(input$clust_type) == "Partitioning Clustering") {
        plot <- fviz_nbclust_plot()
      } else {
        plot <- fviz_dend_plot()
      }

      plot
      #plotly::ggplotly(plot)
    })

    # output$clusters_plot <- plotly::renderPlotly({
    output$clusters_plot <- renderPlot({
      plot <- factoextra::fviz_cluster(
        cluster_res(),
        data = df_cluster(),
        ellipse.type = "convex",
        geom = "point", pointsize = 1,
        ggtheme = theme_minimal()
      )

      plot
      # plotly::ggplotly(plot)
    })

    output$mapping_to_clusters_table <- DT::renderDataTable({

      mapping_to_clusters <- cluster_mapping()

      DT::datatable(mapping_to_clusters,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 10,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 360,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$mapping_from_clusters_table <- DT::renderDataTable({

      cluster_mapping_from <- cluster_mapping() %>%
        dplyr::nest_by(cluster) %>%
        dplyr::mutate(
          indcds = list(sort(unique(data$indcd))),
          indnames = list(sort(unique(data$indname))),
          stkcds = list(sort(unique(data$stkcd))),
          stknames = list(sort(unique(data$stkname)))
        ) %>%
        dplyr::select(-c("data", "indcds", "stkcds"))

      DT::datatable(cluster_mapping_from,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 5,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 180,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })



    # >> Best algorithm output ----
    output$validation_meausre_description <- renderUI({

      # Load description file for measures
      if (req(input$validation_measure) == "internal") {
        desc_file <- app_sys(
          "app/ui/mod_cs_cluster_factoextra/validation_internal.md"
        )
      } else {
        desc_file <- app_sys(
          "app/ui/mod_cs_cluster_factoextra/validation_stability.md"
        )
      }
      includeMarkdown(desc_file)
    })

    output$validation_summary_table <- renderTable({
      optimal_scores <-
        clValid::optimalScores(clValid_res()) %>%
        tibble::as_tibble(rownames = "Measure")

      optimal_scores
    })

    output$validation_meausre_plot <- renderPlot({
      if (req(input$validation_measure) == "internal") {
        measure <- req(input$internal_measure)
      } else {
        measure <- req(input$stability_measure)
      }

      clValid::plot(clValid_res(), measures = measure)
    })

    # >> p-value for HC output ----
    output$pvalue_hc_description <- renderUI({

      # Load description file for pvalue for hc
      desc_file <- app_sys(
        "app/ui/mod_cs_cluster_factoextra/pvalue_hc.md"
      )

      includeMarkdown(desc_file)
    })

    output$pvalue_hc_pick_plot <- renderPlot({

      # Display pick clusters above P-values
      pvhc_res <- pvhc_res()
      plot(pvhc_res, hang = -1, cex = 0.5)
      pvclust::pvrect(
        pvhc_res,
        alpha = req(input$pvhc_alpha)
      )
    })

    output$pvalue_hc_pick_table <- DT::renderDataTable({

      # Map picked clusters to stkcd
      pvhc_res <- pvhc_res()
      pvpick_res <- pvclust::pvpick(
        pvhc_res,
        alpha = req(input$pvhc_alpha)
      )

      pick_pv_clusters <- tibble::tibble(
        id = 1:length(pvpick_res$clusters),
        pick_clusters = pvpick_res$clusters
      )

      pick_pv_clusters <- pick_pv_clusters %>%
        dplyr::rowwise() %>%
        dplyr::mutate(pick_clusters = list(code2name(.data$pick_clusters)))

      DT::datatable(pick_pv_clusters,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 340,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })
  })
}

#' Testing module app of cs_cluster_factoextra
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cluster_factoextra  Testing App of cs_cluster_factoextra.
cs_cluster_factoextra_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  # Filter subset for test
  indcd_group <- c("C38", "C39")
  cs_date <- "2018-12-31"

  csbl_vars <- csbl_vars %>%
    dplyr::filter(.data$indcd %in% indcd_group) %>%
    dplyr::filter(grepl(cs_date, .data$id))


  ui <- fluidPage(
    cs_cluster_factoextra_ui("cs_cluster_factoextra_module")
  )
  server <- function(input, output, session) {
    cs_cluster_factoextra_server(
      "cs_cluster_factoextra_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
