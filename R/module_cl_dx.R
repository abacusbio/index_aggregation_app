clusterDxModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # data selection ?
    # tags$table(
    #   tags$td(checkboxInput(ns("center"), "Center columns (features)", T)),
    #   tags$td(checkboxInput(ns("scale"), "Scale columns (features)", T))
    # ),
    # h4("Upload files (optional)"),
    # wellPanel(
    #   uploadTableModuleUI(ns("upload_index"), "Upload an index table (only has ID and index cols)"),
    #   span(textOutput(ns("error_m_1")), style = "color:salmon"),
    #   uploadTableModuleUI(ns("upload_cl_obj"), "Upload a cluster object .RData file"),
    #   span(textOutput(ns("error_m_2")), style = "color:salmon"),
    #   uploadTableModuleUI(ns("upload_clusters"), "Upload a cluster table"),
    #   span(textOutput(ns("error_m_3")), style = "color:salmon")
    # ),
    # h4("Index & cluster corr"),
    # uiOutput(ns("ui_sel_index")),
    # uiOutput(ns("ui_sel_clusters")),
    h4("Heatmap"),
    wellPanel(
      checkboxInput(ns("show_corr"), "Show index correlation matrix (no plant info)", 
                    value = T),
     # checkboxInput(ns("bi_clust"), "Also cluster plant", value = F),
      actionButton(ns("run_heatmap"), "Show heatmap", icon("running"),
                   class = "btn btn-outline-primary")
    )
  )
}

clusterDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Clustering diagnosis "),
    h2("Heatmap"),
    helpText(
      div("Warning:", class = "text-warning"), #style = "color:orange"),
      "Heatmap is for visualisation. The index dendrogram is derived from the previous
          tab 'Step 1: run cluster' using the chosen input of either the correlation matrix or the
          original data (recommend). If you want to swap the input for clustering, please go back 
          and re-run clustering analyses"
    ),
    shinyjs::hidden(span(id = ns("wait"), p("Plotting...please wait..."), class = "text-danger")),
    div(textOutput(ns("warn_m")), class = "text-warning"),
    plotOutput(ns("plot_heat"), width = "100%", height = "800px"),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("download_heat"))),
      tags$td(downloadModuleUI(ns("download_carpet"), "Download the data")),
      tags$td(downloadModuleUI(ns("download_obj"), "Download the pheatmap object"))
    )
  )
}

#'Cluster diagnosis
#'
#' @param val a reactive value object, containing at least 3 objects: 1) \code{dat}, a data.frame of 
#'        animal by index, e.g. 
#'        reactive(val$dt_index). If the data is index by animal, then \code{transpose} should be 
#'        set to \code{T}. 2) \code{cl_obj}, a class "hclust" or "agnes" object.
#'        3) \code{clusters} a cluster assignment vector. e.g. a \code{cutree} output.
#' @param k a reactive function of an integer. The number of clusters
#' @param center a reactive function of 
#' @return if \code{input$find_k_agg} is on, return texts and graphs to UI, otherwise return a 
#'        dendrogram and a download button to download a cluster result .csv.
#'        , cl$cluster_obj, cl$clusters,
#reactive(input$`find_cl-agg_method`), reactive(input$`find_cl-k_slider`)
clusterDxMod <- function(id, val = NULL, transpose = T, 
                         center = reactive(T), scale = reactive(T),
                         ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("clusterDxMod\n")
      tempVar <- reactiveValues()
      
     # observeEvent(input$show_corr, {shinyjs::toggle(id = "bi_clust", condition = !input$show_corr)})
      
#       # when user starts the app from this step,
#       # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
#       index_user <- uploadTableModuleServer("upload_index", 1, 0)
#       
#       output$error_m_1 <- renderText({
#         validate(
#           need(class(index_user())!="try-error", attr(index_user(), "condition")$message),
#           need(names(index_user())[1]=="ID", "Index file first column should be ID (for plant)")
#         )
#       })
#       
#       observeEvent(length(index_user()) > 0, { # if use index_user, only observe once...
# # cat(" observe index_user\n  val names:");print(names(val))
#         req(class(index_user())=="data.frame") # error_m_1 initial value is NULL, so doesn't work
#         if("dt_index" %in% names(val)) {
#           output$warn_m <- renderText({
#             "You are going to re-write the index table by your uploaded file."
#           })
#         }
#         out <- index_user()[,-1]
#         rownames(out) <- index_user()$ID
#         val$dt_index <- out
#       })
#       
#       cl_obj_user <- uploadTableModuleServer("upload_cl_obj") # RData
#       
#       output$error_m_2 <- renderText({
#         validate(
#           need(class(cl_obj_user())!="try-error", attr(cl_obj_user(), "condition")$message),
#           need(class(cl_obj_user())[1] %in% c("hclust", "agnes"), "Please upload a cluster object")
#         )
#       })
#       
#       observeEvent(length(cl_obj_user()) > 0, {
# # cat(" observe cl_obj_user\n  val names:");print(names(val))        
#         if(!"cl" %in% names(val)) {
#           val$cl <- NULL
#           
#         } else if("cl_obj" %in% names(val$cl)) {
#           output$warn_m <- renderText({
#             "You are going to re-write the clustering object by your uploaded file."
#           })
#         }
#         
#         val$cl$cluster_obj <- cl_obj_user()
#       })
#       
#       clusters_user <- uploadTableModuleServer("upload_clusters", 1, 0)
#       
#       output$error_m_3 <- renderText({
#         validate(
#           need(class(clusters_user())!="try-error", attr(clusters_user(), "condition")$message),
#           need(names(clusters_user())[1]=="Index" && names(clusters_user())[2]=="cluster",
#                "Cluster file headers should be 'Index' 'cluster'")
#         )
#       })
#       
#       observeEvent(length(clusters_user())>0, {
# # cat(" observe clusters_user\n  val names:");print(names(val))        
#         if(!"cl" %in% names(val)) {
#           val$cl <- NULL
#           
#         } else if("clusters" %in% names(val$cl)) {
#           output$warn_m <- renderText({
#             "You are going to re-write the cluster table by your uploaded file."
#           })
#         }
#         
#         out <- clusters_user()[,-1,drop = T]
#         names(out) <- clusters_user()$Index
#         val$cl$clusters <- out
#       })
      
      # # Index and cluster correlations UI
      # output$ui_sel_index <- renderUI({
      #   req(!is.null(val$cl$clusters), !is.null(val$dt_index))
      #   
      #   if(transpose) {x <- t(val$dt_index) } else { x <- val$dt_index} 
      #   
      #   idx <- match(colnames(x), names(val$cl$clusters))
      #   new_name <- paste0(colnames(x), "{", )
      #   
      #   return(selectInput(
      #     session$ns("sel_index"), "Select an index",
      #     names(val$cl$clusters), names(val$cl$clusters)[1],
      #     multiple = T, selectize = F, size = min(8, length(unique(val$cl$clusters)))))
      # })
      # 
      # output$ui_sel_clusters <- renderUI({
      #   req(!is.null(val$cl$clusters))
      #   
      #   return(selectInput(
      #     session$ns("sel_clusters"),  # use session$ns() to get inside id being recognized
      #     "Select cluster(s):", choices = unique(val$cl$clusters),
      #     selected = val$cl$clusters[1], #state_multiple("view_vars", vars, vars),
      #     multiple = TRUE,
      #     selectize = FALSE, size = min(8, length(unique(val$cl$clusters)))
      #   ))
      # })
      # 
      # dt_corr <- reactive({
      #   req(input$sel_index, input$sel_clusters, val$dt_index)
      #   
      #   clstrs <- names(val$dt_index)[grep(1, val$dt_index)]
      #   
      # })
      # 
      # renderDT("idx_clst_corr", val$dt_index[input$sel_index])
      
      # run heatmap
      observeEvent(input$run_heatmap, {
cat(" observe run_heatmap\n  dat:");#print(dim(val$dt_index));cat("  clusters:");print(head(val$cl$clusters));
# cat(" cl_obj:");print(val$cl$cluster_obj$call)
        output$warn_m <- renderText({
          validate(need(!is.null(val$dt_index), "please finish filtering or upload an index"),
                   need(!is.null(val$cl$cluster_obj), 
                        "please finish 'run cluster' or upload a cluster object"),
                   need(!is.null(val$cl$clusters), 
                        "Please finish 'run cluster' or upload a cluster table")
          )
        })
        
        req(val$dt_index, val$cl$cluster_obj, val$cl$clusters) # req(!is.null(dat()))
        
        shinyjs::show("wait")
        
        if(transpose) {x <- t(val$dt_index) } else { x <- val$dt_index} # index x animal
        
        output$plot_heat <- renderPlot({
          withProgress(message = 'Plotting ...',
                       detail = 'This may take a while...', value = 0, {
          req(x, val$cl$cluster_obj, val$cl$clusters)
          # width changes everytime the browser size changes
          tempVar$width  <- session$clientData[[paste0("output_", session$ns("plot_heat"), 
                                                       "_width")]]
          # # gtable
          # # this doesn't require observe input$run_heatmap from the 2nd time...
          # out <- # 28june2021 the order of the vars using Bayer's data seems wrong. Can see 
          #   # neg corrs within a cluster, but the cluster summary shows no neg corr
          #   plotHeatmap(input, output, session,
          #               x, val$cl$cluster_obj, val$cl$clusters, 
          #               transpose = T, center = center(), scale = scale(),
          #               show_corr = reactive(input$show_corr), bi_clust = reactive(input$bi_clust)
          #               # index_sel = reactive(input$sel_index), 
          #               # cluster_sel = reactive(input$sel_clusters)
          #               )
          # tempVar$heatmap <- out$heatmap; tempVar$data <- out$data
# cat(" x:");print(dim(x));print(x[1:3,1:3])
          new_order <- findObsOrder(val$cl$cluster_obj, k = max(val$cl$clusters), desc = F)
# cat(" new_order len:", length(new_order)," ");print(head(new_order))          
          if(isolate(input$show_corr)) {
            x_sorted <- cor(t(x[new_order, ]))
# cat(" x_sorted:");print(dim(x_sorted));print(x_sorted[1:3,1:3])
          } else {
            x_sorted <- x[new_order, ]
          }
          out <- drawHeatMap(x_sorted, "", val$cl$clusters, font_size = 3)
# cat(" tempVar$heatmap:\n");print(class(tempVar$heatmap));print(tempVar$heatmap[-c(1:2)]); # plot(tempVar$heatmap)
# cat("  starts heatmap.2\n"); t <- Sys.time()
          tempVar$heatmap <- out; tempVar$data <- x_sorted
          return(tempVar$heatmap)
          #  return(eval(tempVar$heatmap$call))
          # return(gplots::heatmap.2(x = tempVar$heatmap$x, Rowv = tempVar$heatmap$Rowv, 
          #                          Colv = tempVar$heatmap$Colv, col = tempVar$heatmap$col,
          #                          trace = "none", ColSideColors = tempVar$heatmap$ColSideColors,
          #                          RowSideColors = tempVar$heatmap$RowSideColors))
        }) })
        shinyjs::hide("wait")
# cat("  heatmap.2 finished"); print(Sys.time()-t)
        downloadPlotModuleServer("download_heat", name = "index_heatmap", plots = tempVar$heatmap,
                                 width = reactive(tempVar$width))
        downloadModuleServer("download_carpet", "index_heatmap", tempVar$data,
                             row.names = T, type = "csv")
        downloadModuleServer("download_obj", "index_pheatmap", tempVar$heatmap, type = "rdata")
      }) # observeEvent 
      
      
    })}