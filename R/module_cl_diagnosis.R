clusterDxModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # data selection ?
    # tags$table(
    #   tags$td(checkboxInput(ns("center"), "Center columns (features)", T)),
    #   tags$td(checkboxInput(ns("scale"), "Scale columns (features)", T))
    # ),
    checkboxInput(ns("show_corr"), "Show index correlation matrix (no plant info)", 
                  value = F),
    checkboxInput(ns("bi_clust"), "Also cluster plant", value = F),
    actionButton(ns("run_heatmap"), "Show heatmap", icon("running")),
    # shinyjs::hidden(
    #   div(id = ns("tune"),
    #       h4("Parameter tuning"),      
    #       wellPanel(
    #         checkboxInput(ns("wss"), "Try within-cluster SS measurement (can be slow)", F),
    #         checkboxInput(ns("sil"), "Try Silhouette value measurement (can be slow)", F)
    #       )
    #   )),
    # sliderInput(ns("k_slider"), "Choose # of clusters:", 2, 10, 2, step = 1),
    # selectInput(ns("agg_method"), "Choose an agglomeration method:",
    #             c(# "average",
    #               "single", "complete", "ward", "weighted"), "complete"),
    # checkboxInput(ns("circle"), "Fan shaped dendrogram", F)
  )
}

clusterDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Clustering diagnosis "),
    helpText(
      div("Warning: Heatmap is for visualisation. The index dendrogram is derived from the previous
          tab 'Step 1: run cluster' using the chosen input of either the correlation matrix or the
          original data (recommend). If you want to swap the input for clustering, please go back 
          and re-run clustering analyses", style = "color:orange")
    ),
    # shinyjs::hidden(span(id = ns("wait"), p("Running...please wait..."), style = "color:orange")),
    # shinyjs::hidden(
    #   div(id = ns("parameters"),
    #       h2("Choose parameters by their performance"),
    #       h3("Agglomerative coefficients"),
    #       verbatimTextOutput(ns("message_method")),
    #       h3("Number of clusters (k)"),
    #       h4("by tree merging steps"),
    #       verbatimTextOutput(ns("message_h")),
    #       fluidRow(
    #         column(6,
    #                h4("by largest total within-cluster sum of squares drop"),
    #                #  textOutput(ns("method")),
    #                plotOutput(ns("plot_tss"))
    #         ),
    #         column(6,
    #                h4("by largest mean within-cluster silhouette value"),
    #                #   textOutput(ns("method")),
    #                plotOutput(ns("plot_sil"))
    #         )
    #       )
    #   )),
    h2("Heatmap"),
   plotOutput(ns("plot_heat"), width = "100%", height = "800px"),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("download_heat"))),
      tags$td(downloadModuleUI(ns("download_carpet"), "Download the data"))
    )
  )
}

#'Cluster diagnosis
#'
#' @param val a reactive value object
#' @param dat a reactive function with data.frame of animal by index in it, e.g. 
#'        reactive(val$dt_index). If the data is index by animal, then \code{transpose} should be 
#'        set to \code{T}. 
#' @param cl_obj a reactive function with a class "hclust" or "agnes" object in it.
#' @param clusters a reactive function of a cluster assignment vector in it. e.g. a \code{cutree}
#'        output.
#' @param k a reactive function of an integer. The number of clusters
#' @param center a reactive function of 
#' @return if \code{input$find_k_agg} is on, return texts and graphs to UI, otherwise return a 
#'        dendrogram and a download button to download a cluster result .csv.
#'        , cl$cluster_obj, cl$clusters,
#reactive(input$`find_cl-agg_method`), reactive(input$`find_cl-k_slider`)
clusterDxMod <- function(id, val = NULL, dat, cl_obj, clusters, transpose = T, 
                         center = reactive(T), scale = reactive(T),
                          ...) {
  moduleServer(
    id,
    function(input, output, session) {

      tempVar <- reactiveValues()
      
      observeEvent(input$run_heatmap, {
cat("clusterDxMod\n observe button, dat:");print(dim(dat()));#cat(" cl_obj");print(cl_obj)
# cat(" clusters:");print(head(clusters))
        req(dat, cl_obj, clusters) # req(!is.null(dat()))
        x <- t(dat()) # index x animal
        
        output$plot_heat <- renderPlot({
          # width changes everytime the browser size changes
          tempVar$width  <- session$clientData[[paste0("output_", session$ns("plot_heat"), "_width")]]

          # gtable
          tempVar$heatmap <-
            plotHeatmap(input, output, session,
                        x, cl_obj, clusters, transpose = T, center = center(), scale = scale(),
                      show_corr = reactive(input$show_corr), bi_clust = reactive(input$bi_clust))
# cat(" tempVar$heatmap:\n");print(class(tempVar$heatmap));print(tempVar$heatmap[-c(1:2)]); #plot(tempVar$heatmap)
# cat("  starts heatmap.2\n"); t <- Sys.time()
          return(tempVar$heatmap)
#           return(pheatmap::pheatmap(mat = tempVar$heatmap$x, color = tempVar$heatmap$col,
#                                     cluster_rows = tempVar$heatmap$Rowv, cluster_cols = tempVar$heatmap$Colv,
#                                     cutree_cols = tempVar$heatmap$ColSideColors,
#                                     cutree_rows = tempVar$heatmap$RowSideColors))

# test = matrix(rnorm(200), 20, 10)
# test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
# test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
# test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
# colnames(test) = paste("Test", 1:10, sep = "")
# rownames(test) = paste("Gene", 1:20, sep = "")
# return(pheatmap::pheatmap(test))
          #  return(eval(tempVar$heatmap$call))
          # return(gplots::heatmap.2(x = tempVar$heatmap$x, Rowv = tempVar$heatmap$Rowv, 
          #                          Colv = tempVar$heatmap$Colv, col = tempVar$heatmap$col,
          #                          trace = "none", ColSideColors = tempVar$heatmap$ColSideColors,
          #                          RowSideColors = tempVar$heatmap$RowSideColors))
          })
# cat("  heatmap.2 finished"); print(Sys.time()-t)
        # downloadPlotModuleServer("download_heat", "heatmap_index", reactive(tempVar$heatmap),
        #                          tempVar$width)
        # downloadModuleServer("download_carpet", "heatmap_index", x, type = "csv")
        })
      
      
    })}