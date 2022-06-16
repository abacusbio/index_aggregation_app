#' Summarize 1) correlations?, 2) classification variable pattern
#' 
#' 
clusterSumStatModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional)"),
    wellPanel(
      uploadTableModuleUI(ns("upload_index"), "index table (only has ID and index cols)"),
      span(textOutput(ns("error_m_1")), style = "color:salmon"),
      # uploadTableModuleUI(ns("upload_cl_obj"), "cluster object .RData file"),
      # span(textOutput(ns("error_m_2")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_clusters"), "cluster table"),
      span(textOutput(ns("error_m_3")), style = "color:salmon")
    ),
    h4("Table and plot control"),
    wellPanel(
      numericInput(ns("view_dec"), "Decimals", 3, 0, 10, 1), 
     # numericInput(ns("show_n_indexes"), "# indexs to show", 10, 1, 10, 1),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
     )
  )
}

clusterSumStatModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Cluster Summary statistics"),
    h2("Within-cluster correlations"),
    h3("Table"),
    textOutput(ns("warn_m")),
    textOutput(ns("error_m")),
    renderTableModuleUI(ns("sum_cor")),
    br(),br(),
    h3("Histogram"),
    plotOutput(ns("hist_cor"), height = "600px"),
    downloadPlotModuleUI(ns("dnld_hist_cor"))
  )
}

#'Cluster summary statistics
#'
#' @param val a reactive value object, containing at least 3 objects: 1) \code{dat}, a data.frame of 
#'        animal by index, e.g. 
#'        reactive(val$dt_index). If the data is index by animal, then \code{transpose} should be 
#'        set to \code{T}. 2) \code{cl_obj}, a class "hclust" or "agnes" object.
#'        3) \code{clusters} a cluster assignment vector. e.g. a \code{cutree} output.
#' @param cl a reactive function of a list \code{cl}.
#' @param k a reactive function of an integer. The number of clusters
#' @param center a reactive function of 
#' @return if \code{input$find_k_agg} is on, return texts and graphs to UI, otherwise return a 
#'        dendrogram and a download button to download a cluster result .csv.
#'        , cl$cluster_obj, cl$clusters,
clusterSumStatMod <- function(id, val = NULL, cl,
                              transpose = T,  center = reactive(T), scale = reactive(T),
                              val_report, report_prefix = NA,
                         ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("clusterSumStatMod\n")
      # initlialize
      tempVar <- reactiveValues()
      
      observeEvent(input$show_corr, { shinyjs::toggle(id = "bi_clust", condition = !input$show_corr) })
      
      # when user starts the app from this step,
      # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
      index_user <- uploadTableModuleServer("upload_index", 1, 0)
      
      output$error_m_1 <- renderText({
        validate(
          need(class(index_user())!="try-error", attr(index_user(), "condition")$message),
          need(names(index_user())[1]=="ID", "Index file first column should be ID (for plant)")
        )
      })
      
      observeEvent(length(index_user()) > 0, { # if use index_user, only observe once...
        # cat(" observe index_user\n  val names:");print(names(val))        
        if("dt_index" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the index table by your uploaded file."
          })
        }
        out <- index_user()[,-1]
        rownames(out) <- index_user()$ID
        val$dt_index <- out
      })
      
      # cl_obj_user <- uploadTableModuleServer("upload_cl_obj") # RData
      # 
      # output$error_m_2 <- renderText({
      #   validate(
      #     need(class(cl_obj_user())!="try-error", attr(cl_obj_user(), "condition")$message),
      #     need(class(cl_obj_user())[1] %in% c("hclust", "agnes"), "Please upload a cluster object")
      #   )
      # })
      # 
      # observeEvent(length(cl_obj_user()) > 0, {
      #   # cat(" observe cl_obj_user\n  val names:");print(names(val))        
      #   if(!"cl" %in% names(val)) {
      #     val$cl <- NULL
      #     
      #   } else if("cl_obj" %in% names(val$cl)) {
      #     output$warn_m <- renderText({
      #       "You are going to re-write the clustering object by your uploaded file."
      #     })
      #   }
      #   
      #   val$cl$cluster_obj <- cl_obj_user()
      # })
      
      clusters_user <- uploadTableModuleServer("upload_clusters", 1, 0)
      
      output$error_m_3 <- renderText({
        validate(
          need(class(clusters_user())!="try-error", attr(clusters_user(), "condition")$message),
          need(names(clusters_user())[1]=="Index" && names(clusters_user())[2]=="cluster",
               "Cluster file headers should be 'Index' 'cluster'")
        )
      })
      
      observeEvent(length(clusters_user())>0, {
        # cat(" observe clusters_user\n  val names:");print(names(val))        
        if(!"cl" %in% names(val)) {
          val$cl <- NULL
          
        } else if("clusters" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the cluster table by your uploaded file."
          })
        }
        
        out <- clusters_user()[,-1,drop = T]
        names(out) <- clusters_user()$Index
        val$cl$clusters <- out
      })
      
      # Index and cluster correlations
      output$error_m <- renderText({
        validate(need(!is.null(val$dt_index), "please finish filtering or upload an index"),
                 # need(!is.null(val$cl$cluster_obj), 
                 #      "please finish 'run cluster' or upload a cluster object"),
                 need(!is.null(val$cl$clusters), 
                      "Please finish 'run cluster' or upload a cluster table")
        )
      })
      
      # observeEvent(input$run_heatmap, {
      sth <- eventReactive({cl()
      #  clusters_user()
        }, {
        # reactive({
cat("clusterSumStatMod\n reactive sth\n")
          # cl()
        req(length(input$error_m)==0, 
            !is.null(val$dt_index), !is.null(val$cl$clusters)) #, !is.null(val$cl$cluster_obj)
        
        shinyjs::show("wait")
        
        if(transpose) {x <- t(val$dt_index) } else { x <- as.matrix(val$dt_index)} # index x animal
        index_cor <- cor(x, use = "pairwise.complete.obs", method = "pearson")
# cat("  x:");print(dim(x));print(x[1:3,1:3])
# cat("  index_cor:");print(dim(index_cor));print(index_cor[1:3,1:3])
# cat("  clusters:", length(val$cl$clusters));print(head(val$cl$clusters))
        df_cor <- do.call(rbind, lapply(unique(val$cl$clusters), function( i ){
          
          idx <- match(names(val$cl$clusters)[grep(i, val$cl$clusters)], colnames(index_cor))
          cor_sub <- index_cor[idx, idx][upper.tri(index_cor[idx, idx])]
          if(length(cor_sub)==0) { # only 1 obs in this cluster
            cor_sub <- 1
          }
          return(data.frame(cor = cor_sub, cluster = i, n_index = length(idx)))
        }))
        tempVar$df_cor <- df_cor

        sum_cor <- dplyr::group_by(df_cor, cluster) %>% 
          dplyr::summarise(n_corr = as.integer(n()),
                           mean = mean(cor, na.rm = T), median = median(cor, na.rm = T),
                           sd = sd(cor, na.rm = T), 
                           min = min(cor, na.rm = T), max = max(cor, na.rm = T))

        missing <- dplyr::group_by(df_cor, cluster) %>% 
          summarise(n_corr_missing = sum(is.na(cor)))
        
        sum_cor <- dplyr::left_join(sum_cor, missing, by = "cluster") %>% 
          dplyr::right_join(dplyr::select(df_cor, cluster, n_index) %>% distinct())
        
        val_report[[paste0(report_prefix, "cor_table")]] <- sum_cor
        
        return(sum_cor)
        })
      
      observeEvent({sth()
        input$view_dec}, {
# cat(" renderTable\n")        
        renderTableModuleServer("sum_cor", sth, extensions = "FixedHeader",
                                downloadName = "index_cor_summary", 
                                digits = reactive(input$view_dec))
      })
      
      output$hist_cor <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(tempVar$df_cor)
cat(" renderPlot\n  df_cor:\n");print(head(tempVar$df_cor))
        width  <- session$clientData[[paste0("output_", session$ns("hist_cor"), 
                                             "_width")]]
        
        plot_hist <- plotHist(input, output, session,
                              tempVar$df_cor, "cor", "cluster", 
                              font_size = reactive(input$font_size))
        
        downloadPlotModuleServer("dnld_hist_cor", "histogram_index_by_cluster",
                                 plot_hist, reactive(width))
        
        val_report[[paste0(report_prefix, "cor_p")]] <- plot_hist
        
        return(plot_hist)
      }) })
    })}