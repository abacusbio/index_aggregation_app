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
      selectInput(ns("sel_cluster"), "Or select all indexes from a cluster", choices = ""),
      shinyjs::hidden(
        div(id = ns("top"),
         # tags$table(
          #  tags$td(
         sliderInput(ns("sel_top_n"), "Select top # individuals", 1, 10, 10, 1),
           # tags$td(
         checkboxInput(ns("percent"), "Use percentage", F, width = "30%")#)
          #)
        )),
      actionButton(ns("run_heatmap"), "Run analysis", icon("running"), 
                   class = "btn btn-primary")
      ),
    h4("Plot control"),
    numericInput(ns("font_size"), "Font size", 5, 1, 20, 1)
    )
}

aggDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Aggregated index diagnosis"),
    textOutput(ns("error_m")),
    h2("Heatmap"),
    plotOutput(ns("plot_cor")),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_heat"))),
      tags$td(downloadModuleUI(ns("dnld_cor"), "Download the correlation"))
      ),
    br(),
    h2(textOutput(ns("top_n_title"))),
    h3("Table"),
    renderDtTableModuleUI(ns("top_n_index")),
    br(),
    h3("Bar chart"),
    plotOutput(ns("plot_top_n"))
  )
}

#' Diagnosis tools for aggregated indexes
#'
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{reactive(
#'        val$dt_index)}, a data.frame of animal by index. If the data is index by animal, 
#'        then \code{transpose} should be set to \code{T}.
#'        2) \code{cl}, a list containing \code{cl_obj}, a class "hclust" or "agnes" object,
#'        and \code{val$cl$clusters}, a cluster assignment vector. e.g. a \code{cutree} 
#'        output. 4) \code{val$dt_desc_ev_clean}, a data.frame of 2 columns: column_labelling and 
#'        classifier/ 5) \code{val$dt_ev_clean}, a data.frame of columns Index and trait names.
#'        6) 
#' @param dt_ev_agg a reactive function of a data.frame of columns Index, cluster and trait names
#'        assignments
#' 
#' @return 
aggDxMod <- function(id, val, transpose = F, clusters = reactive(NULL), dt_ev_agg = reactive(NULL),
                     dt_index = reactive(NULL),
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
        updateSelectInput(session, "sel_cluster", choices = c("", unique(clusters())))
        updateSelectInput(session, "sel_index", 
                          choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
      })
      
      observeEvent(!is.null(dt_ev_agg()), { 
        updateSelectInput(session, "sel_agg", choices = dt_ev_agg()$Index)
      })
      
      observeEvent(!is.null(dt_index()) | !is.null(val$dt_index), {
# cat(" observe val$dt_index: ", class(dt_index()));print(dim(dt_index()))
# cat(class(val$dt_index));print(dim(val$dt_index))
        # req(!is.null(dt_index()))
# cat("  req met\n")
        updateSliderInput(session, "sel_top_n", 
                          max = ifelse(transpose, ncol(dt_index()), nrow(dt_index())))
      }, ignoreInit = T)
      
      observeEvent(input$percent, {
        req(!is.null(dt_index()))
        
        if(isTRUE(input$percent)) {
          updateSliderInput(session, "sel_top_n", max = 100)
        } else {
          updateSliderInput(session, "sel_top_n", 
                            max = ifelse(transpose, ncol(dt_index()), nrow(dt_index())))
        }
      })
      
      observeEvent(isTRUE(input$sel_index!="" || input$sel_cluster!=""), { # react even when False!
# cat(" observe sel_index or sel_cluster not empty\n")        
       if(input$sel_index=="" && input$sel_cluster == "") {
         shinyjs::hide("top")
        } else {
        shinyjs::show("top")
        }
      }, ignoreInit = T)
      
      # CALCULATE AGGREGATED INDEXES FOR EACH ANIMAL
      observeEvent( #input$run_heatmap,{
        !is.null(dt_ev_agg()) && !is.null(val$dt_ebv_filtered) && !is.null(val$dt_description_clean),
        {
cat(" observe 3 data\n")#, names val: ");print(names(val))
# cat("  dt_ev_agg:");print(dim(dt_ev_agg()))
# cat("  ebv_filtered:");print(dim(val$dt_ebv_filtered))
# cat("  description_clean:");print(dim(val$dt_description_clean))
# cat("  desc_ev_clean:");print(dim(val$dt_desc_ev_clean))
# cat("  dt_index:");print(dim(val$dt_index))
          req(!is.null(dt_ev_agg()), !is.null(val$dt_ebv_filtered), !is.null(clusters()),
              !is.null(val$dt_description_clean),
              !is.null(val$dt_desc_ev_clean), !is.null(dt_index()))
          
          if(transpose) {
            index <- t(dt_index())
            } else { index <- dt_index() }
# cat("  index: ", class(index));print(dim(index));print(index[1:3,1:3])
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
          dt_sub_index_ids_orig <- data.frame(ID = rownames(index), index)

          val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig, by = "ID")

          # update
          # animal ID x Index
          val$dt_index <- dplyr::select(
            val$dt_sub_index_ids, dplyr::any_of(c(dt_ev_agg()$Index, val$dt_ev_filtered$Index)))
cat("  val$dt_index dim: ");print(dim(val$dt_index))
      }, ignoreInit = T) # observe 3 datasets
      
      # CALCULATE
      observeEvent(input$run_heatmap,{
cat(" observe run_heatmap val$dt_index:");print(dim(val$dt_index))
# cat("  sel_agg:");print(input$sel_agg);cat("  sel_index:", input$sel_index, " sel_cluster:",
# input$sel_cluster, " sel_top_n:", input$sel_top_n, " percent:", input$percent, " dt_ev_agg: ");
# print(dim(dt_ev_agg()))
# cat("  clusters:", class(clusters()), length(clusters()), " ncol dt_index:", ncol(val$dt_index),
    # " len dt_ev_agg$Index:", length(dt_ev_agg()$Index), "\n")
        req(input$sel_agg!="", input$sel_top_n, # input$percent, # doesn't work if ==F
            ncol(val$dt_index)>=length(clusters())+length(dt_ev_agg()$Index))

        sel_index <- sapply(strsplit(input$sel_index, "\\{"), head, 1) %>% unlist()
        
        if(input$sel_cluster!="") {
          sel_cluster <- names(clusters())[grep(input$sel_cluster, clusters())]
        } else {
          sel_cluster <- "#"
        }
        
        tempVar$sel_index <- sel_index
        tempVar$sel_cluster <- sel_cluster
# cat("  sel_index:");print(sel_index);cat("  sel_cluster:");print(sel_cluster)        
        if(sel_cluster!="#") {
          df_index_sub <- dplyr::select(val$dt_index, dplyr::any_of(c(input$sel_agg, sel_cluster)))
          
        } else {
          df_index_sub <- dplyr::select(val$dt_index, dplyr::any_of(c(input$sel_agg, sel_index)))
        }
# cat("  df_index_sub:");print(dim(df_index_sub));print(head(df_index_sub))
        # find n
        if(!input$percent) {
          n <- input$sel_top_n
        } else {
          n <- min(round(nrow(df_index_sub)*input$percent/100, 0), nrow(df_index_sub))
        }
        tempVar$n <- n

        output$top_n_title <- renderText({
          req(n)

          return(
            paste0("Top ", n, ifelse(input$percent, "%", ""), " individual agreement among indexes")
          )
        })
        
        # ranking agreement
        df_top_n <- lapply(names(df_index_sub), function( i ){ # by index
          
          idx <- order(df_index_sub[,i], decreasing = T)
          out <- data.frame(order = 1:length(idx), Index = rep(i, length(idx)),
                            plant = rownames(df_index_sub)[idx], value = df_index_sub[idx,i])
          return(head(out, n))
        })
        names(df_top_n) <- names(df_index_sub)
        tempVar$df_top_n <- df_top_n
# cat("  df_top_n:", class(df_top_n));print(dim(df_index_sub));print(df_top_n[[1]][1,])
# cat("  table_top_n list:")
        table_top_n <- do.call(cbind, lapply(df_top_n, function(i) {
          out <- i[, c("plant", "value")]
          # names(out) <- paste0(i$Index[1], "_", names(out))
          return(out)
        }))
        table_top_n <- cbind(order = 1:n, table_top_n)
# cat("  table_top_n2:");print(dim(df_index_sub));print(head(table_top_n[,]))
        renderDtTableModuleServer("top_n_index", reactive(table_top_n), digits = reactive(3),
                                  downloadName = paste0("top_", n, ifelse(input$percent, "_%", ""),
                                                       "_by_index"))
        
        # correlation
        if(ncol(df_index_sub) >= 2) { # only calculate corrlations when >=2 vars are selected

          df_cor <- do.call(cbind, lapply(df_top_n, function(i) return(i$value)))
          tempVar$corr <- cor(df_cor, use = "pairwise.complete.obs")
        } # if ncol df_index_sub >=2

      }) # observe run_heatmap
      
      # Heatmap
      output$plot_cor <- renderPlot({
        req(length(tempVar$corr) > 1)
        
       # font_size <- reactive(input$font_size)()
        
        tempVar$width  <- session$clientData[[paste0("output_", session$ns("plot_cor"), 
                                                     "_width")]]
        cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256)
        tempVar$heatmap <- pheatmap::pheatmap(tempVar$corr, color = rev(cols),
                                              cluster_cols = F, cluster_rows = F, 
                                              fontsize = input$font_size)
        return(tempVar$heatmap)
      })
      
      downloadPlotModuleServer("dnld_heat", 
                               name = paste0("heatmap_", input$sel_agg, "_", tempVar$sel_index,
                                             "_", tempVar$sel_cluster),
                               plots = tempVar$heatmap, width = reactive(tempVar$width))
      
      downloadModuleServer("dnld_cor", 
                           paste0("correlation_", input$sel_agg, "_", input$sel_index),
                           tempVar$corr, T)
      
      # TOP N individual agreement plot
      output$plot_top_n <- renderPlot({
        req(tempVar$df_top_n, tempVar$sel_index, tempVar$sel_cluster, input$font_size)
        
        tempVar$width_top  <- session$clientData[[paste0("output_", session$ns("plot_top_n"), 
                                                     "_width")]]
        if(tempVar$sel_cluster!="#") {
          tempVar$plot_top_n <- 
            plotTopNbar(input, output, session,
                        tempVar$df_top_n, tempVar$sel_cluster[1], reactive(input$font_size))
        } else {
          tempVar$plot_top_n <- 
            plotTopNbar(input, output, session,
                        tempVar$df_top_n, tempVar$sel_index, reactive(input$font_size))
        }
        
        return(tempVar$plot_top_n)
      })
      
      downloadPlotModuleServer("dnld_plot_top", 
                               name = paste0("top_", tempVar$n, "_", input$sel_agg, "_", 
                                             tempVar$sel_index, "_", tempVar$sel_cluster),
                               plots = tempVar$plot_top_n, width = reactive(tempVar$width_top))
      # downloadModuleServer("dnld_top", 
                           # paste0("top_", tempVar$n, "_", input$sel_agg, "_", tempVar$sel_index, 
                                  # "_", tempVar$sel_cluster),
                           # tempVar$df_top_n, T, "csv")
    })}