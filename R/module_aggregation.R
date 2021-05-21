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
    wellPanel(
      #numericInput(ns("show_n_indexes"), "# indexs to show", 10, 1, 10, 1),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
      ),
    )
}

aggDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Aggregated index diagnosis"),
    textOutput(ns("error_m")),
    h2(textOutput(ns("corr_title"))),
    h3("Scatter/heatmap plot"),
    plotOutput(ns("plot_cor")),# height = "800px"),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_heat"))),
      tags$td(downloadModuleUI(ns("dnld_cor"), "Download the correlation"))
      ),
    br(),
    h3("Minimum correlation given a quantile"),
    numericInput(ns("quantile"), "Enter a quantile", 50, 1, 100, 1),
    renderDtTableModuleUI(ns("quantile_table")),
    br(),br(),
    h2(textOutput(ns("top_n_title"))),
    h3("Table"),
    renderDtTableModuleUI(ns("top_n_index")),
    br(),br(),
    h3("Scatter plot"),
    plotOutput(ns("plot_top_n")),# height = "800px"),
    downloadPlotModuleUI(ns("dnld_plot_top"))
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
#'        classifier/ 5) \code{val$dt_ev_filtered}, a data.frame of columns Index, classVar
#'        (optional) and trait names
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
     # plot_height <- reactive(input$plot_height)
      # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
      ebv_desc_user <- uploadTableModuleServer("upload_ebv_desc")
      
      output$error_m_0 <- renderText({
        validate(
          need(class(ebv_desc_user())!="try-error", attr(ebv_desc_user(), "condition")$message),
          need(names(ebv_desc_user())[1]=="column_labelling", 
               "Description file column header wrong."),
          need("EBV" %in% ebv_desc_user()[,2,drop = T], "Are you sure this is an EBV description file?")
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
     # observeEvent(!is.null(clusters()), {
      #  req(val$dt_ev_filtered)
        
       # updateSelectInput(session, "sel_cluster", choices = c("", unique(clusters())))
        #updateSelectInput(session, "sel_index", 
         #                 choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
      # })
      
      observeEvent(!is.null(dt_ev_agg()), { 
        req(clusters, val$dt_ev_filtered, val$dt_desc_ev_clean)
        
        updateSelectInput(session, "sel_index", 
                          choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
        updateSelectInput(session, "sel_agg", choices = dt_ev_agg()$Index)
        
        if("ClassVar" %in% val$dt_desc_ev_clean$classifier) { # add user defined group into sel_cluster
          
          group_headers <- val$dt_desc_ev_clean$column_labelling[
            val$dt_desc_ev_clean$classifier=="ClassVar"]
          
          group_levels <- sapply(val$dt_ev_filtered[,group_headers,drop = F], unique) # list
          group_levels <- append(group_levels, list("cluster" = unique(clusters())))
          
          updateSelectInput(session, "sel_cluster", c("", group_levels))
      
        } else { # use cluster results directly
          
          updateSelectInput(session, "sel_cluster", choices = c("", unique(clusters())))
        }
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
        updateNumericInput(session, "show_n_indexes",
                           max = max(length(input$sel_index),length(input$sel_cluster)))
        
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
# cat(" observe 3 data\n")#, names val: ");print(names(val))
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
# cat("  val$dt_index dim: ");print(dim(val$dt_index))
      }, ignoreInit = T) # observe 3 datasets
      
      # CALCULATE
      observeEvent(input$run_heatmap,{
# cat(" observe run_heatmap val$dt_index:");print(dim(val$dt_index))
# cat("  sel_agg:");print(input$sel_agg);cat("  sel_index:", input$sel_index, " sel_cluster:",
# input$sel_cluster, " sel_top_n:", input$sel_top_n, " percent:", input$percent, " dt_ev_agg: ");
# print(dim(dt_ev_agg()))
# cat("  clusters:", class(clusters()), length(clusters()), " ncol dt_index:", ncol(val$dt_index),
    # " len dt_ev_agg$Index:", length(dt_ev_agg()$Index), "\n")
        req(input$sel_agg!="", input$sel_top_n, # input$percent, # doesn't work if ==F
            ncol(val$dt_index)>=length(clusters())+length(dt_ev_agg()$Index))

        sel_index <- sapply(strsplit(input$sel_index, "\\{"), head, 1) %>% unlist()
        
        if(input$sel_cluster[1]!="") {
          sel_cluster <- names(clusters())[grep(input$sel_cluster, clusters())]
        } else {
          sel_cluster <- "#"
        }
        
        tempVar$sel_index <- sel_index
        tempVar$sel_cluster <- sel_cluster
# cat("  sel_index:");print(sel_index);cat("  sel_cluster:");print(sel_cluster)        
        if(sel_cluster[1]!="#") {
          df_index_sub <- dplyr::select(val$dt_index, dplyr::any_of(c(input$sel_agg, sel_cluster)))
          
        } else {
          df_index_sub <- dplyr::select(val$dt_index, dplyr::any_of(c(input$sel_agg, sel_index)))
        }
# cat("  df_index_sub:");print(dim(df_index_sub));print(head(df_index_sub))
        
        # correlation
        if(ncol(df_index_sub) >= 2) { # only calculate corrlations when >=2 vars are selected
          
          tempVar$corr <- cor(df_index_sub, use = "pairwise.complete.obs")
        } # if ncol df_index_sub >=2
        
        # find n
        if(!input$percent) {
          n <- input$sel_top_n
        } else {
          n <- min(round(nrow(df_index_sub)*input$percent/100, 0), nrow(df_index_sub))
        }
        tempVar$n <- n
        
        output$corr_title <- renderText({
          req(input$sel_index!="" || input$sel_cluster!="")
          
          if(input$sel_cluster!="") {
            return(paste0("Distribution of correlations with aggregated indexes: cluster ",
                          input$sel_cluster, " indexes"))
          } else {
            return(paste0("Distribution of correlations with aggregated indexes: ",
                          input$sel_index, " indexes"))
          }
        })

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
cat("  df_top_n:", class(df_top_n));print(dim(df_index_sub));print(tempVar$df_top_n[[1]][1,])
# cat("  table_top_n list:")
        table_top_n <- do.call(cbind, lapply(df_top_n, function(i) {
          out <- i[, c("plant", "value")]
          # names(out) <- paste0(i$Index[1], "_", names(out))
          return(out)
        }))
        table_top_n <- cbind(order = 1:n, table_top_n)
# cat("  table_top_n2:");print(dim(df_index_sub));print(head(table_top_n[,]))
        renderDtTableModuleServer("top_n_index", reactive(table_top_n),
                                  downloadName = paste0("top_", n, ifelse(input$percent, "%", ""),
                                                       "_by_index"))
        
        }) # observe run_heatmap
      
      # Heatmap or scatter plot
      output$plot_cor <- renderPlot({
        req(length(tempVar$corr) > 1, input$font_size)
        
        # initial parameters
        n <- 30
        width  <- session$clientData[[paste0("output_", session$ns("plot_cor"), 
                                                     "_width")]]
        cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256)
        
        # whent there are too many indexes, draw a bar chart instead of a heatmap
        if(length(tempVar$corr) < 11) {
          heat_map <- pheatmap::pheatmap(tempVar$corr, 
                                         color = rev(cols), 
                                         # breaks = seq(-1, 1, length.out = 256),
                                         cluster_cols = F, cluster_rows = F, 
                                         fontsize = input$font_size)
          
        } else {
          heat_map <- plotcorrDot(input, output, session,
                                  tempVar$corr, reactive(input$sel_agg),
                                  font_size = reactive(input$font_size))
        }
        
        downloadPlotModuleServer("dnld_heat", 
          name = paste0("heatmap_", 
                        paste0(input$sel_agg, collapse = "_"),
                        "_", tempVar$sel_index,
                        "_", input$sel_cluster),
          plots = heat_map$p # if(class(heat_map)[1]=="list") {
          #   gridExtra::grid.arrange(grobs = heat_map, ncol = 1)} else {heat_map}, 
          # width = reactive(width)
          )
        
        tempVar$corr_df <- heat_map$df  # Index aggregated_index correlation id
        
        downloadModuleServer("dnld_cor", 
                             paste0("correlation_", paste0(input$sel_agg, collapse = "_"), "_",
                                    tempVar$sel_index, "_",
                                    input$sel_cluster),
                             tempVar$corr, T)
        
        return( heat_map$p
          # if(class(heat_map)[1]=="list") {
          # gridExtra::grid.arrange(grobs = heat_map, ncol = 1)} else {heat_map}
          )
      }) #, height = 800) # can't use reactive values for height
      
      # Correlation by quantile table
      q_table <- #eventReactive(input$quantile | input$run_heatmap, {
        reactive({
cat(" eventReactive quantile\n input$quantile: ", input$quantile, "tempVar$corr_df:\n")
# print(head(tempVar$corr_df))
        req( length(tempVar$corr_df) >1, input$quantile)  # Index aggregated_index correlation id

        n <- round(max(tempVar$corr_df$id)*input$quantile/100, 0)
        out <- dplyr::filter(tempVar$corr_df, id == n) %>% 
          dplyr::select(-Index)
        names(out)[which(names(out)=="id")] <- "n_indexes"
        
        return(out)
      })
      
      renderDtTableModuleServer("quantile_table", q_table, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = paste0("min_corr_at_", input$quantile, "%"))
      
      # TOP N individual agreement plot
      output$plot_top_n <- renderPlot({
        req(tempVar$df_top_n, input$font_size)
    
        # initial parameters
        n <- 30
        width_top  <- session$clientData[[paste0("output_", session$ns("plot_top_n"), 
                                                     "_width")]]
        
        l <- plotTopNdot(input, output, session,
                          tempVar$df_top_n, reactive(input$sel_agg),
                          reactive(input$font_size))
        
        tempVar$top_n_df <- l$df # Index   aggregated_index    n   percent id
          
        downloadPlotModuleServer(
          "dnld_plot_top", 
          name = paste0("top_", tempVar$n, "_", paste0(input$sel_agg, collapse = "_"), "_", 
                        tempVar$sel_index, "_", input$sel_cluster),
          plots = l$p, # if(class(plot_top_n)[1]=="list") {
            # gridExtra::grid.arrange(grobs = plot_top_n, ncol = 1)} else {plot_top_n},
          width = reactive(width_top))
        
        # if("ggplot2" %in% class(plot_top_n)) {
          return(l$p)
          
        # } else if(class(plot_top_n)[1]=="list") { #if (length(plot_top_n)==1) {
          # return(gridExtra::grid.arrange(grobs = plot_top_n, ncol = 1))
        # }
      })#, height = 800)
      
     # downloadModuleServer("dnld_top", 
                           # paste0("top_", tempVar$n, "_", input$sel_agg, "_", tempVar$sel_index, 
                                  # "_", tempVar$sel_cluster),
                           # tempVar$df_top_n, T, "csv")
    })}