clusteringSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # data selection ?
    checkboxInput(ns("find_k_agg"), "Calculate optimal # of clusters and agglomeration method", 
                  value = F),
    actionButton(ns("run_cluster"), "Run clustering"),
    tags$table(
      tags$td(checkboxInput(ns("center"), "Center columns (features)", T)),
      tags$td(checkboxInput(ns("scale"), "Scale columns (features)", T))
    ),
    sliderInput(ns("k_slider"), "Choose # of clusters:", 2, 10, 2, step = 1),
    selectInput(ns("agg_method"), "Choose an agglomeration method:",
                c("average", "single", "complete", "ward", "weighted"), "complete"),
    checkboxInput(ns("circle"), "Fan shaped dendrogram", F)
  )
}

clusteringUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Clustering formation "),
    shinyjs::hidden(
      div(id = ns("parameters"),
          h2("Choose parameter by their performance"),
          h3("Agglomerative coefficients"),
          verbatimTextOutput(ns("message_method")),
          h3("Number of clusters k value"),
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
    plotOutput(ns("plot_dendro"), width = "100%", height = "800px")
  )
}
#'
#'@param dat a reactive function with data.frame of index by animal in it
clusteringMod <- function(id, val = reactive(NULL),
                          dat = reactive(), cor_mat = F, 
                          ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("clusteringMod\n")
      tempVar <- reactiveValues()
      
      observeEvent(dat, { # may not work
        k_max <- nrow(dat()) - 1
        updateSliderInput(session, "k_slider", max = k_max)  
      })
      
      observeEvent(input$find_k_agg, {
       if(isTRUE(input$find_k_agg)) {
         shinyjs::show("parameters")
       } else {
         shinyjs::hide("parameters")
       }
      })
      
      observeEvent(input$run_cluster, {
cat(" observe run_cluster");print(dim(dat()))        
        req(!is.null(dat()))
        
        output$method <- renderText({paste0("(", input$agg_method, ")")})
        
        if(input$find_k_agg) { # Maybe change to warning message
         
          # Find optimal agglomeriative method
          cl <- runCluster(dat(), cor_mat, input$scale, input$center) # !!!
          cl$cluster_obj
          cl$best_method # agglomeration method
          cl$agg_coefs # agglomerative coefficients
          
          output$message_method <- renderPrint({
            paste0("The best method is ", cl$best_method, " with an agglomerative coefficients of ", 
                   cl$agg_coefs[cl$best_method])
          }) # textoutput
          
          updateSelectInput(session, "agg_method", selected = cl$best_method)
          
          # Find best k
          # list(k_h, k_tss, k_sil, h, p_tss, p_sil)
          op_cut <- reactive({
            findOptimalCut(dat(), cl$cluster_obj, hc_method = input$agg_method)
          })
          
          output$message_h <- renderPrint({
            paste0("The best k is ", op_cut()$k_h, " at the largest height change of ", op_cut()$h)
          })
          
          output$plot_tss <- renderPlot({
            print(op_cut()$p_tss)
          })
          
          output$plot_sil <- renderPlot({
            print(op_cut()$p_sil)
          })
          
         # cutree(cl$cluster_obj, k = op_cut)
           
        } else {
          # list(cluster_obj, clusters)
          cl <- runFinalCluster(dat(), cor_mat = F, cluster_object = NULL,
                                scale = input$scale, center = input$center, k = input$k_slider,
                                best_method = input$agg_method)
        }
        
        # dendrograph
        output$plot_dendro <- renderPlot({
          drawDendro(as.hclust(cl$cluster_obj), cl$clusters, circle = input$circle)
        })
        
      }) # observe run_cluster
      
  
      })}