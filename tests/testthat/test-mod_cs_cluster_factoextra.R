# Tests for module of cs_cluster_factoextra  ----
# context("Tests for module of cs_cluster_factoextra")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()

# Set up test environment

# Prepare test data
csbl_vars <- load_csbl_vars(use_online_data = FALSE)

# Filter subset for test
indcd_group <- c("C38", "C39")
cs_date <- "2018-12-31"

csbl_vars <- csbl_vars %>%
  dplyr::filter(.data$indcd %in% indcd_group) %>%
  dplyr::filter(grepl(cs_date, .data$id))

test_that("cs_cluster_factoextra_server - reactives and output updates", {
  testServer(cs_cluster_factoextra_server,
    args = list(csbl_vars = reactive(csbl_vars)),
    {

      # cs_cluster_factoextra_server with typical user inputs ====

      # >>Check df_cluster_raw()----
      df_cluster_raw <- df_cluster_raw()
      expect_s3_class(df_cluster_raw, "data.frame")
      expect_fields <- c("indcd")
      actual_fields <- names(df_cluster_raw)
      expect_true(all(expect_fields %in% actual_fields))
      expect_true(tibble::has_rownames(df_cluster_raw))

      # >>Check df_cluster() ----
      df_cluster <- df_cluster()
      expect_s3_class(df_cluster, "data.frame")
      expect_true(all(purrr::map_lgl(df_cluster, .f = is.numeric)))
      expect_true(tibble::has_rownames(df_cluster))

      # >>Check df_cluster_indcd() ----
      df_cluster_indcd <- df_cluster_indcd()
      expect_s3_class(df_cluster_indcd, "data.frame")
      expect_fields <- c("indcd")
      actual_fields <- names(df_cluster_indcd)
      expect_true(all(expect_fields %in% actual_fields))
      expect_true(tibble::has_rownames(df_cluster_indcd))

      # >>Check dist() ----
      dist_metric <- c(
        "euclidean", "manhattan", "pearson",
        "spearman", "kendall"
      )
      dist_params <- tidyr::expand_grid(dist_metric)
      dist_results <- dist_params %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dist = list({
          session$setInputs(
            dist_metric = .data$dist_metric,
          )
          dist()
        }))
      dist_results %>%
        dplyr::summarise(
          expect_results =
            list({
              expect_s3_class(.data$dist, class = "dist")
            }),
          .groups = "keep"
        )

      # >>Check hopkins_stats() ----
      hopkins_stats <- hopkins_stats()
      expect_true(dplyr::between(hopkins_stats, 0, 1))

      # Check nbclust_res()
      suppressWarnings({
        # Disable output during executing tests
        withr::with_output_sink(tempfile(), {
          session$setInputs(
            dist_metric = "euclidean",
            nbclust_method = "kmeans"
          )
          nbclust_res <- nbclust_res()
        })
      })
      expect_type(nbclust_res, type = "list")
      expect_fields <- c(
        "All.index", "All.CriticalValues",
        "Best.nc", "Best.partition"
      )
      actual_fields <- names(nbclust_res)
      expect_equal(expect_fields, actual_fields)

      # Check cluster_res()
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
      hc_method <- c(
        "ward.D", "single", "complete",
        "average", "mcquitty", "median", "centroid",
        "ward.D2"
      )
      cluster_pc_params <- tidyr::expand_grid(
        clust_algorithm = c(
          pc_algs
        ),
        cluster_k = 2,
        # Not used by PC, just set for typical value for input control.
        hc_linkage_method = "ward.D"
      )
      cluster_hc_params <- tidyr::expand_grid(
        clust_algorithm = c(
          hc_algs
        ),
        cluster_k = 2,
        # Only use a typical value for input control, beacuse computation
        # on all combinations among algorithms and methods will consume to
        # much time.
        hc_linkage_method = "ward.D"
      )
      cluster_params <- dplyr::bind_rows(
        cluster_pc_params, cluster_hc_params
      )

      cluster_results <- cluster_params %>%
        dplyr::rowwise() %>%
        dplyr::mutate(cluster_res = list({
          session$setInputs(
            clust_algorithm = .data$clust_algorithm,
            cluster_k = .data$cluster_k,
            hc_linkage_method = .data$hc_linkage_method
          )
          cluster_res()
        }))

      cluster_results %>%
        dplyr::summarise(
          expect_results = list({
            expect_type(.data$cluster_res, "list")
            expect_fields <- c("res", "cluster", "data")
            actual_fields <- names(.data$cluster_res)
            expect_true(all(expect_fields %in% actual_fields))
            expect_true(NROW(.data$cluster_res$cluster) ==
              NROW(.data$cluster_res$data))
          }),
          .groups = "keep"
        )

      # >>Check cluster_mapping() ----
      session$setInputs(
        clust_algorithm = "K-Means:kmeans()",
        cluster_k = 2
      )
      cluster_mapping <- cluster_mapping()
      expect_s3_class(cluster_mapping, "data.frame")
      expect_fields <- c("stkcd", "stkname", "indcd", "indname", "cluster")
      actual_fields <- names(cluster_mapping)
      expect_true(all(expect_fields %in% actual_fields))

      # >>Check clValid_res() ----
      validation_clmethod <- list(c(
        "kmeans", "pam", "clara",
        "hierarchical", "agnes", "diana"
      ))
      validation_dist_metric <- c("euclidean", "correlation", "manhattan")
      validation_method <- c("ward", "single", "complete", "average")
      validation_measure <- c("internal", "stability")
      clValid_params <- tidyr::expand_grid(
        validation_clmethod = validation_clmethod,
        validation_dist_metric = "euclidean",
        validation_method = "average",
        validation_measure = validation_measure
      )

      clValid_results <- clValid_params %>%
        dplyr::rowwise() %>%
        dplyr::mutate(clValid_res = list({
          suppressMessages({
            session$setInputs(
              validation_clmethod = .data$validation_clmethod,
              validation_dist_metric = .data$validation_dist_metric,
              validation_method = .data$validation_method,
              validation_measure = .data$validation_measure
            )
            clValid_res()
          })
        }))


      clValid_results %>%
        dplyr::summarise(
          expect_results = list({
            expect_s4_class(.data$clValid_res, "clValid")
          }),
          .groups = "keep"
        )


      # >>Check pvhc_res() ----

      pvhc_dist_metric <- c(
        "correlation", "uncentered", "abscor",
        "euclidean", "maximum", "manhattan",
        "canberra", "binary", "minkowski"
      )
      pvhc_method <- c(
        "ward.D", "ward.D2", "single", "complete", "average",
        "mcquitty", "median", "centroid"
      )

      pvhc_params <- tidyr::expand_grid(
        # Only use typical value because all combination consume too much time.
        pvhc_dist_metric = "correlation",
        pvhc_method = "average"
      )

      pvhc_results <- pvhc_params %>%
        dplyr::rowwise() %>%
        dplyr::mutate(pvhc_res = list({
          suppressWarnings({
            withr::with_output_sink(tempfile(), {
              session$setInputs(
                pvhc_dist_metric = .data$pvhc_dist_metric,
                pvhc_method = .data$pvhc_method
              )
              pvhc_res()
            })
          })
        }))

      pvhc_results %>%
        dplyr::summarise(
          expect_results = list({
            expect_s3_class(.data$pvhc_res, "pvclust")
          }),
          .groups = "keep"
        )
    }
  )
})

test_that("cs_cluster_factoextra_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_cs_cluster_factoextra_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ncs_cluster_factoextra_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # cs_cluster_factoextra_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `cs_cluster_factoextra_module-factor_groups` = "Financial Risk",
    #   `cs_cluster_factoextra_module-factors_in_group` = select_factors,
    #   `cs_cluster_factoextra_module-cs_cluster_factoextra` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
