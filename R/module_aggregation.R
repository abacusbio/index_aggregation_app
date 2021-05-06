aggDxModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional)"),
    wellPanel(
      uploadTableModuleUI(ns("upload_ebv_desc"), "EBV description file"),
      div(textOutput(ns("error_m_0")), class = "text-danger"),
      uploadTableModuleUI(ns("upload_ebv"), "EBV file"),
      textOutput(ns("error_m_1"))
    ),
    h4("Main"),
    wellPanel(
      selectInput(ns("sel_agg"), "Select an aggregated index", choice = "", multiple = T),
      selectInput(ns("sel_index"), "Select an original index", choices = ""),
      actionButton(ns("run_heatmap"), "Show heatmap", icon("running"), 
                   class = "btn btn-primary")
      ),
    )
}

aggDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Aggregated index diagnosis"),
    textOutput(ns("error_m")),
    h2("Heatmap"),
    plotOutput(ns("plot_cor"))
  )
}

#' Diagnosis tools for aggregated indexes
#'
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{reactive(
#'        val$dt_index)}, a data.frame of animal by index. If the data is index by animal, 
#'        then \code{transpose} should be 
#'        set to \code{T}. 2) \code{cl}, a list containing \code{cl_obj}, a class "hclust" or 
#'        "agnes" object. 3) \code{val$cl$clusters}, a cluster assignment vector. e.g. a \code{cutree} 
#'        output. 4) \code{val$dt_desc_ev_clean}, a data.frame of 2 columns: column_labelling and 
#'        classifier/ 5) \code{val$dt_ev_clean}, a data.frame of columns Index and trait names.
#'        6) \code{val$dt_ev_agg}, a data.frame of columns Index, cluster and trait names
#' @param dt_ev_agg a reactive function of a data.frame of columns Index, cluster and trait names
#'        assignments
#' 
#' @return 
aggDxMod <- function(id, val, transpose = T, clusters = reactive(NULL), dt_ev_agg = reactive(NULL), 
                     ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("aggDxMod\n")
      # INITIALIZE
      tempVar <- reactiveValues()
      
      # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
      ebv_desc_user <- uploadTableModuleServer("upload_ebv_desc")
      
      output$error_m_0 <- renderText({
        validate(
          need(class(ebv_desc_user())!="try-error", attr(ebv_desc_user(), "condition")$message),
          need(names(ebv_desc_user())[1]=="column_labelling", "Description file column header wrong.")
        )
      })
      
      observeEvent(length(ebv_desc_user()) > 0, { # if use ev_desc_user, only observe once...
        req(class(ebv_desc_user())=="data.frame")
        
        if("dt_description_clean" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the EBV description table by your uploaded file."
          })
        }
        val$dt_description_clean <- ebv_desc_user()
      })
      
      ebv_user <- uploadTableModuleServer("upload_ebv", 1, 0)
      
      output$error_m_1 <- renderText({
        validate(
          need(class(ebv_user())!="try-error", attr(ebv_user(), "condition")$message),
          need(names(ebv_user())[1]=="ID",
               "EBV file headers should be 'ID' and trait names")
        )
      })
      
      observeEvent(length(ebv_user())>0, {
        req(class(ebv_user())=="data.frame", !is.null(val$dt_description_clean),
            names(ebv_user())[1]=="ID")
        if("dt_ebv_filtered" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the EV table by your uploaded file."
          })
        }
        req(length(input$warn_m)==0)
        
        val$dt_ebv_filtered <- cleanEbvData(val$dt_description_clean, ebv_user())
      })
      
      output$error_m <- renderText({
# cat(" erro_m:");print(input$error_m) # always NULL
# cat("  names val:");print(names(val))
        validate(need(!is.null(val$dt_desc_ev_clean), "Please upload an EV description file"),
                 need(!is.null(val$dt_description_clean), "Please upload an EBV description file"),
                 need(!is.null(val$dt_ebv_filtered), "Please upload an EBV file"),
                 # need(!is.null(val$dt_index), "Please finish filtering or upload an index table"),
                # need(!is.null(clusters()), #val$cl$clusters), 
                 #     "Please finish 'run cluster' or upload a cluster table"),
                 need(!is.null(dt_ev_agg()), "Please finish 'Make new weights'"),
                 need(length(input$sel_index) + length(input$sel_agg) >= 2, 
                 "Please select at least 2 indexes")
        )
      })
      
      # UPDATE SELECTION INPUT
      observeEvent(!is.null(clusters()), {
        
        updateSelectInput(session, "sel_index", 
                          choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
      })
      
      
      observeEvent(!is.null(dt_ev_agg()), { 
        updateSelectInput(session, "sel_agg", choices = dt_ev_agg()$Index)
      })
      
      # CALCULATE AGGREGATED INDEXES FOR EACH ANIMAL
      observeEvent( #input$run_heatmap,{
        !is.null(dt_ev_agg()) && !is.null(val$dt_ebv_filtered) && !is.null(val$dt_description_clean),
        {
# cat(" observe 3 data\n")#, names val: ");print(names(val))
# cat("  dt_ev_agg:");print(dim(dt_ev_agg()))
# cat("  ebv_filtered:");print(dim(val$dt_ebv_filtered))
# cat("  description_clean:");print(dim(val$dt_description_clean))
# cat("  desc_ev_clean:");print(dim(val$dt_desc_ev_clean))
# cat("  dt_index:");print(dim(val$dt_index))
          req(!is.null(dt_ev_agg()), !is.null(val$dt_ebv_filtered), 
              !is.null(val$dt_description_clean),
              !is.null(val$dt_desc_ev_clean), !is.null(val$dt_index))
  
          # dt_ev_agg: Index, cluster and traits
          dt_sub_ebv_index_ids <- 
            calculateIndividualBW(input, output, session,
                                  val$dt_ebv_filtered, dt_ev_agg(), val$dt_description_clean,
                                  val$dt_desc_ev_clean)

          dt_sub_index_ids <- # ID sex new_index_1 ...
            dt_sub_ebv_index_ids[,!names(dt_sub_ebv_index_ids) 
                                     %in% val$dt_description_clean$column_labelling[
                                       val$dt_description_clean$classifier=="EBV"] ]

          # update
          # ID sex new_index_1 ... new_index_3, index_1 ... index_3000
          dt_sub_index_ids_orig <- data.frame(ID = rownames(val$dt_index), val$dt_index)

          val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig, by = "ID")

          # update
          # animal ID x Index
          val$dt_index <- dplyr::select(
            val$dt_sub_index_ids, dplyr::any_of(c(dt_ev_agg()$Index, val$dt_ev_filtered$Index)))
# cat("  val$dt_index dim: ");print(dim(val$dt_index))
      }, ignoreInit = T) # observe 3 datasets
      
      # RENDER CORRELATIONS AS USER CHOOSES
      observeEvent(input$run_heatmap,{
# cat(" observe run_heatmap val$dt_index:");print(dim(val$dt_index))
# cat(" sel_agg:");print(input$sel_agg);cat(" sel_index:", input$sel_index, "\n")
        req(input$sel_agg, ncol(val$dt_index)>=length(clusters())+length(dt_ev_agg()$Index))

        sel_index <- sapply(strsplit(input$sel_index, "\\{"), head, 1) %>% unlist()
        out <- dplyr::select(val$dt_index, dplyr::any_of(c(input$sel_agg, sel_index)))

        if(ncol(out) >= 2) {
          tempVar$corr <- cor(out, use = "pairwise.complete.obs")
# cat("  corr:");print(tempVar$corr)
        }

        output$plot_cor <- renderPlot({
          req(length(tempVar$corr) > 1)
        
          tempVar$width  <- session$clientData[[paste0("output_", session$ns("plot_dendro"), 
                                                       "_width")]]
          cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256)
          tempVar$heatmap <- pheatmap::pheatmap(tempVar$corr, color = rev(cols),silent = T)
          return(tempVar$heatmap)
        })
      }) # observe run_heatmap
      
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
    })}