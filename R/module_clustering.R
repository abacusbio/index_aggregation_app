clusteringModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # data selection ?
    shinyjs::disabled(
      div(id = ns("bttn"),
          actionButton(ns("run_cluster"), "Run clustering", icon("running"), 
                       class = "btn btn-primary")
      )),
    br(),br(),
    h4("Parameter tuning"),
    wellPanel(
      checkboxInput(ns("find_k_agg"), 
                    strong("Find the optimal # of clusters and agglomeration method"), 
                    value = F),
    shinyjs::hidden(
      div(id = ns("tune"),
            # h4("Parameter tuning"),
            checkboxInput(ns("wss"), "Try within-cluster SS measurement (can be slow)", F),
            checkboxInput(ns("sil"), "Try Silhouette value measurement (can be slow)", F)
        ))
    ),
   h4("Main"),
   wellPanel(
     selectInput(ns("which_data"), "Choose input data", 
                 choices = c("Index by individual", "Index correlation matrix"), 
                 selected = "Index by individual"),
     shinyjs::hidden(
       div(id = ns("absolute_cor"), 
           checkboxInput(ns("absolute"), "Absolute correlation", F)) 
     ),
     tags$table(
       tags$td(checkboxInput(ns("center"), "Center columns (features)", T)),
       tags$td(checkboxInput(ns("scale"), "Scale columns (features)", T))
     ),
     sliderInput(ns("k_slider"), "Choose # of clusters:", 2, 10, 2, step = 1),
     selectInput(ns("agg_method"), "Choose an agglomeration method:",
                 c(# "average",
                   "single", "complete", "ward", "weighted"), "complete")
   ),
   checkboxInput(ns("circle"), "Fan shaped dendrogram", F)
  )
}

clusteringModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Clustering formation "),
    shinyjs::hidden(span(id = ns("wait"), p("Running...please wait..."), class = "text-danger")),
    shinyjs::hidden(
      div(id = ns("parameters"),
          h2("Choose parameters by their performance"),
          h3("Agglomerative coefficients"),
          verbatimTextOutput(ns("message_method")),
          h3("Number of clusters (k)"),
          h4("by tree merging steps"),
          verbatimTextOutput(ns("message_h")),
          fluidRow(
            column(6,
              h4("by largest total within-cluster sum of squares drop"),
            #  textOutput(ns("method")),
              plotOutput(ns("plot_tss"))
              ),
            column(6,
              h4("by largest mean within-cluster silhouette value"),
           #   textOutput(ns("method")),
              plotOutput(ns("plot_sil"))
              )
          )
          )),
    h2("Final Clustering"),
    plotOutput(ns("plot_dendro"), width = "100%", height = "800px"),
    downloadPlotModuleUI(ns("dnld_dendro")),
    downloadModuleUI(ns("dnld_cluster"), "Download the clusters table"),
    downloadModuleUI(ns("dnld_cl"), "Download the cluster object"),
    downloadModuleUI(ns("dnld_ev"), "Download the updated EV file"),
    downloadModuleUI(ns("dnld_desc_ev"), "Download the updated EV description file")
  )
}

#'
#' @param dat a reactive function with data.frame of animal by index in it. If the data is index by
#'       animal, then \code{transpose} should be set to \code{T} 
#' @param col_sel a reactive function. The col names of the \code{dat} to be selected for clustering 
#'       analysis. 
#' @param val a reactive value containing 1) \code{val$dt_desc_ev_clean}, a data.frame of 2 columns:
#'        column_labelling and classVar, 2) \code{val$dt_ev_filtered}, a data.frame of columns Index
#'        , classVar cols, and trait cols.
#' @return if \code{input$find_k_agg} is on, return texts and graphs to UI, otherwise return a 
#'        dendrogram and a download button to download a cluster result .csv from generated 
#'        val$cl.
clusteringMod <- function(id, val, dat, #= reactive(NULL),
                         # col_sel = reactive(NULL), 
                         transpose = F,
                          ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("clusteringMod\n")
      req(val, dat)
      tempVar <- reactiveValues()
      
      observeEvent(!is.null(dat() ), { # update k slider max value
        if(transpose) { k_max <- ncol(dat()) - 1 } else { k_max <- nrow(dat()) - 1 }
        updateSliderInput(session, "k_slider", max = min(k_max, 10))
        shinyjs::enable("bttn")
      }, ignoreInit = T)
      
      observeEvent(input$find_k_agg, {
       if(isTRUE(input$find_k_agg)) {
         shinyjs::show("parameters")
         shinyjs::show("tune")
       } else {
         shinyjs::hide("parameters")
         shinyjs::hide("tune")
       }
      })
      
      observeEvent(input$which_data, {
        if(input$which_data == "Index correlation matrix") {
          shinyjs::show("absolute_cor")
        } else {
          shinyjs::hide("absolute_cor")
        }
      })
      
      observeEvent(input$run_cluster, {
# cat(" observe run_cluster\n  dat:");#print(dim(dat()));cat("  val:");print(names(val))
# cat("  input$which_data: ", input$which_data, ", input$absolute: ", input$absolute, "\n")
        req(!is.null(dat()), "dt_index" %in% names(val), !is.null(val$dt_ev_filtered),
            !is.null(val$dt_desc_ev_clean))
        
        shinyjs::show("wait")
        shinyjs::disable("bttn")
#         if(length(col_sel()) > 0) {
#           indexes <- col_sel()
# # cat(" length desc file > 0\n  desc:");print(head(col_sel()));cat("indexes:");print(head(indexes))
#           dt <- dplyr::select(dat(), matches(indexes))
# # cat("  dt:");print(dim(dt))
#         }
        if(transpose) {
          dt <- data.frame(t(dat())) # colnames are lost
        } else {dt <- dat()}
        
        cor_mat <- ifelse(input$which_data == "Index correlation matrix", T, F)
        
        output$method <- renderText({paste0("(", input$agg_method, ")")})
        
        # Tune parameters K and agglomerative coefficient
        if(input$find_k_agg) { # Maybe change to warning message
          
          # Find optimal agglomeriative method
          cl <- runCluster(dt, cor_mat, input$absolute, input$scale, input$center, 
                           n_core = max(4, parallel::detectCores()-4))
          # !!! takes very long time with 2999 indexes

          # cl$cluster_obj
          # cl$best_method # agglomeration method
          # cl$agg_coefs # agglomerative coefficients
          output$message_method <- renderPrint({
            paste0("The best method is ", cl$best_method, " with an agglomerative coefficients of ", 
                   cl$agg_coefs[cl$best_method])
          }) # textoutput
    
          updateSelectInput(session, "agg_method", selected = cl$best_method)
t <- Sys.time()
          # Find best k
          # list(k_h, k_tss, k_sil, h, p_tss, p_sil)
          op_cut <- reactive({
            findOptimalCut(dt, cl$cluster_obj, hc_method = input$agg_method, 
                           wss = isolate(input$wss), silhouette = isolate(input$sil))
          })
cat(" Done findoptimalcut ");print(Sys.time()-t)
          output$message_h <- renderPrint({
            paste0("The best k is ", op_cut()$k_h, " at the largest height change of ", op_cut()$h)
          })
          
          output$plot_tss <- renderPlot({
            withProgress(message = 'Plotting ...',
                         detail = 'This may take a while...', value = 0, {
            print(op_cut()$p_tss)
          }) })
          
          output$plot_sil <- renderPlot({
            withProgress(message = 'Plotting ...',
                         detail = 'This may take a while...', value = 0, {
            print(op_cut()$p_sil)
          }) })
          
          shinyjs::hide("wait")
           
        } else { # run with known k and ac
          
          shinyjs::show("wait")
          # list(cluster_obj, clusters)
          cl <- runFinalCluster(dt, cor_mat = F, cluster_object = NULL,
                                scale = input$scale, center = input$center, k = input$k_slider,
                                best_method = input$agg_method)
          val$cl <- cl # 20april2021
# cat(" else runFinalCluster\n  val$cl:");print(names(val$cl))
          # create files to download
          tempVar$dt_index_new <- data.frame(Index = names(cl$clusters), cluster = cl$clusters) %>% 
            right_join(val$dt_ev_filtered, by = "Index")
          
          tempVar$dt_desc_ev_clean <- rbind(val$dt_desc_ev_clean, c("cluster", "Group"))
          
          # dendrograph
          output$plot_dendro <- renderPlot({
            withProgress(message = 'Plotting ...',
                         detail = 'This may take a while...', value = 0, {
            tempVar$width  <- session$clientData[[paste0("output_", session$ns("plot_dendro"), 
                                                         "_width")]]
            tempVar$plot <- drawDendro(as.hclust(cl$cluster_obj), cl$clusters, circle = input$circle)
            return(tempVar$plot)
          }) })
          
          # download clustering objects
          downloadPlotModuleServer("dnld_dendro", 
                                   name = paste0("dendrogram_k", input$k_slider, "_aggMethod_",
                                                 input$agg_method),
                                   plots = tempVar$plot, width = reactive(tempVar$width))
          downloadModuleServer("dnld_cl", 
                               paste0("cluster_object_k", input$k_slider, "_aggMethod_",
                                      input$agg_method),
                               cl$cluster_obj, F, "rdata")
          downloadModuleServer("dnld_cluster", 
                               downloadName = paste0("clusters_k", input$k_slider, "_aggMethod_",
                                                     input$agg_method), 
                               data.frame(Index = names(cl$clusters), cluster = cl$clusters),
                               F, "csv")
          downloadModuleServer("dnld_ev", 
                               downloadName = paste0("updated_ev_k", input$k_slider, "_aggMethod_",
                                                     input$agg_method), 
                               tempVar$dt_index_new, F, "csv")
          downloadModuleServer("dnld_desc_ev", 
                               downloadName = paste0("updated_ev_desc_k", input$k_slider, "_aggMethod_",
                                                     input$agg_method), 
                               tempVar$dt_desc_ev_clean, F, "csv")
          shinyjs::hide("wait")
          # return(cl) # 
        #  val[["cluster"]] <- cl$clusters
        } # if find k agg
        shinyjs::enable("bttn")
      }) # observe run_cluster
      
      return(reactive(val$cl))
      })}