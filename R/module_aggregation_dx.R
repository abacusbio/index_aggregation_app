aggDxModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional)"),
    wellPanel(
      uploadTableModuleUI(ns("upload_ebv_desc"), "EBV description file"),
      div(textOutput(ns("error_m_0")), class = "text-danger"),
      uploadTableModuleUI(ns("upload_ebv"), "EBV file"),
      div(textOutput(ns("error_m_1")), class = "text-danger"),
      uploadTableModuleUI(ns("upload_benchmark"), "Benchmark index EV file (optional)"),
      div(textOutput(ns("error_m_2")), class = "text-danger")
    ),
    h4("All index"),
    wellPanel(
      selectInput(ns("sel_benchmark"), "Select a benchmark index", 
                  choice = c("", "average"))
    ),
    h4("User select indexes"),
    wellPanel(
      selectInput(ns("sel_agg"), "Select an aggregated index", choice = "", multiple = T),
      selectInput(ns("sel_index"), "Select an original index", choices = ""),
      selectInput(ns("sel_cluster"), "Or select all indexes from a cluster", choices = ""),
      actionButton(ns("run_heatmap"), "Run analysis", icon("running"), 
                   class = "btn btn-primary")
      ),
    h4("Plot control"),
    wellPanel(
      # numericInput(ns("show_n_indexes"), "# indexs to show", 10, 1, 10, 1),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
      )
    )
}

aggDxModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Index correlations"),
    h2("Distribution of correlations between within-cluster indexes and their aggregated index"),
    h3("Scatter plot"),
    textOutput(ns("error_m")), # input file missing
    textOutput(ns("warn_m")), # input file format wrong
    plotOutput(ns("plot_cor_default")),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_heat_default"))),
      tags$td(downloadModuleUI(ns("dnld_cor_default"), "Download the correlation"))
    ),
    br(),br(),
    h2(textOutput(ns("corr_title"))),
    h3("Scatter plot"), #"Scatter/heatmap plot"),
    textOutput(ns("erro_m_a")),
    plotOutput(ns("plot_cor")),# height = "800px"),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_heat"))),
      tags$td(downloadModuleUI(ns("dnld_cor"), "Download the correlation"))
      ),
    br(),br(),
    h3("Minimum correlation given a quantile"),
    numericInput(ns("quantile"), "Enter a quantile", 50, 1, 100, 1),
    renderDtTableModuleUI(ns("quantile_table"))
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
#'        classifier/ 5) \code{val$dt_ev_filtered}, a data.frame of columns Index, ClassVar
#'        (optional) and trait names
#' @param dt_ev_agg a reactive function of a data.frame of columns Index, cluster and trait names
#'        assignments
#' 
#' @return df_for_dx2, a reactive function of a data.frame to be used for aggDxMod2
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
        } else {
          output$warn_m <- renderText({
            sanityCheckEBVdesc(ebv_desc_user())
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
        } else {
          output$warn_m <- renderText({
            sanityCheckEBV(ebv_user(), val$dt_description_clean)
          })
        }
        
        val$dt_ebv_filtered <- cleanEbvData(val$dt_description_clean, ebv_user())
      })
      
      bnchmrk_user <- uploadTableModuleServer("upload_benchmark", 1, 0)
      
      output$error_m_2 <- renderText({
        validate(
          need(class(bnchmrk_user())!="try-error", attr(bnchmrk_user(), "condition")$message),
          need(names(bnchmrk_user())[1]=="Index",
               "Benchmark EV file headers should be 'Index' and trait names")
        )
      })
      
      observeEvent(length(bnchmrk_user())>0, {
        req(class(bnchmrk_user())=="data.frame", !is.null(val$dt_description_clean),
            names(bnchmrk_user())[1]=="Index")
        if("dt_bnchmrk_ev_cleaned" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the benchmark EV table by your uploaded file."
          })
        } else {
          output$warn_m <- renderText({
            sanityCheckEVbenchmark(bnchmrk_user(), val$dt_ev_filtered)
          })
        }
        
        val$dt_bnchmrk_ev_cleaned <- cleanEVplant(val$dt_desc_ev, bnchmrk_user())
        updateSelectInput(session, "sel_benchmark", choices = c("", "average", "upload"))
      })
      
      # show on top
      output$error_m <- renderText({
# cat(" erro_m:");print(input$error_m) # always NULL
# cat("  names val:");print(names(val))
        validate(need(!is.null(val$dt_description_clean), "Please upload an EBV description file"),
                 need(!is.null(val$dt_desc_ev_clean), "Please upload an EV description file"),
                 need(!is.null(val$dt_ebv_filtered), "Please upload an EBV file"),
                 need(!is.null(val$dt_ev_filtered), "Please upload an EV file"),
                 need(!is.null(val$dt_index), "Please finish filtering or upload an index table"),
                 need(!is.null(clusters()), #val$cl$clusters),
                     "Please finish 'run cluster' or upload a cluster table"),
                 need(!is.null(dt_ev_agg()), "Please finish 'Make new weights'")
        )
      })
      
      # show mid-page
      output$error_m_a <- renderText({
        validate(need(length(input$sel_agg) > 0, "Please select at least 1 aggregated index"),
                 need(length(input$sel_index) >0 || length(input$sel_cluster > 0), 
                      "Please select at least 1 index or cluster")
        )
      })
      
      # UPDATE SELECTION INPUT
     observeEvent(!is.null(clusters()), {
       req(val$dt_ev_filtered)
        
       updateSelectInput(session, "sel_cluster", choices = c("", unique(clusters())))
        updateSelectInput(session, "sel_index",
                         choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
      })
      
      observeEvent(!is.null(dt_ev_agg()), { 
# cat(" observe dt_ev_agg: ");print(dim(dt_ev_agg()))
        req(clusters, val$dt_ev_filtered, val$dt_desc_ev_clean)
# cat("  req satisfied. clusters:");print(head(clusters()))
        updateSelectInput(session, "sel_index", 
                          choices = c("", paste0(names(clusters()), "{", clusters(), "}")))
        updateSelectInput(session, "sel_agg", choices = dt_ev_agg()$Index)
        
        if("Group" %in% val$dt_desc_ev_clean$classifier) { # add user defined group into sel_cluster

          group_headers <- val$dt_desc_ev_clean$column_labelling[
            val$dt_desc_ev_clean$classifier=="Group"]
          
          group_levels <- sapply(val$dt_ev_filtered[,group_headers,drop = F], unique, simplify = F)
          group_levels <- append(group_levels, list("cluster" = unique(clusters())))
          
          updateSelectInput(session, "sel_cluster", choices = group_levels)

        } else { # use cluster results directly
# cat("  no user defined group\n  ")       
          updateSelectInput(session, "sel_cluster", choices = c("", unique(clusters())))
        }
      })
       
      # CALCULATE AGGREGATED INDEXES FOR EACH ANIMAL
      observeEvent( #input$run_heatmap,{
        !is.null(dt_ev_agg()) && !is.null(val$dt_ebv_filtered) && !is.null(val$dt_description_clean),
        {
# cat(" observe 3 data\n")#, names val: ");print(names(val))
# cat("  dt_ev_agg:");print(dim(dt_ev_agg()));print(dt_ev_agg())
# cat("  ebv_filtered:");print(dim(val$dt_ebv_filtered))
# cat("  description_clean:");print(dim(val$dt_description_clean))
# cat("  desc_ev_clean:");print(dim(val$dt_desc_ev_clean))
# cat("  dt_index:");print(dim(val$dt_index));print(val$dt_index[1:3,1:3]);print(names(val$dt_index))
          req(!is.null(dt_ev_agg()), !is.null(val$dt_ebv_filtered), !is.null(clusters()),
              !is.null(val$dt_description_clean),
              !is.null(val$dt_desc_ev_clean), !is.null(dt_index()))
          
          # avoid recalculation
          if(sum(!is.na(match(dt_ev_agg()$Index, colnames(val$dt_index_new)))) == 0) {
            
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
# cat("  dt_sub_index_ids:");print(dim(dt_sub_index_ids));print(head(dt_sub_index_ids))
            # update
            # ID sex new_index_1 ... new_index_3, index_1 ... index_3000
            dt_sub_index_ids_orig <- data.frame(ID = rownames(index), index, check.names = F)
# cat("  dt_sub_index_ids_orig:");print(dim(dt_sub_index_ids_orig));print(dt_sub_index_ids_orig[3:5,3:5])
            val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig, by = "ID")
# cat("  dt_sub_index_ids:");print(dim(val$dt_sub_index_ids));print(val$dt_sub_index_ids[3:5,3:5])
            # update
            # animal ID x Index
            val$dt_index_new <- dplyr::select(
              val$dt_sub_index_ids, dplyr::any_of(c(dt_ev_agg()$Index, val$dt_ev_filtered$Index)))
cat("  val$dt_index_new dim: ");print(dim(val$dt_index_new));print(val$dt_index_new[3:5,dt_ev_agg()$Index])
            rownames(val$dt_index_new) <- val$dt_sub_index_ids$ID
          } # if new_index? not in val$dt_index_new
        }, ignoreInit = T) # observe 3 datasets
      
      # CALCULATE DEFAULT CORRELATION
      cor_default <- eventReactive(
        {length(val$dt_index_new) > 0
          input$sel_benchmark
          }, 
        {
# cat("event reactive val$dt_index_new\n")
# cat("  dt_index_new:");print(dim(val$dt_index_new));print(val$dt_index_new[1:3, dt_ev_agg()$Index])
        req(!is.null(clusters()), !is.null(dt_ev_agg()), !is.null(val$dt_index))
        
        by_cluster <- do.call(rbind, lapply(unique(clusters()), function(i) {
          
          sel_agg <- grep(as.character(i), dt_ev_agg()$Index, value = T)
          sel_cluster <- names(clusters())[grep(i, clusters())]
          df_index_sub <- dplyr::select(val$dt_index_new, 
                                        dplyr::any_of(c(sel_agg, sel_cluster)))
          m <- cor(df_index_sub, use = "pairwise.complete.obs")
          out <- makeLongCor(input, output, session,
                             m, reactive(sel_agg)) # Index aggregated_index correlation id
          return(out)
        }))
        
        by_cluster <- dplyr::arrange(by_cluster, desc(correlation)) %>% 
          mutate(id = dplyr::row_number())  # sort the id differently
                 # index_type = "aggregation") # Index aggregated_index correlation id
cat("  val$dt_index_new:");print(val$dt_index_new[1:3,1:3])        
        index <- dplyr::select(val$dt_index_new, !matches("new_index_"))
        dt_sub_index_ids_orig <- data.frame(ID = rownames(index), index, check.names = F)
cat("  dt_sub_index_ids_orig:");print(dim(dt_sub_index_ids_orig));print(dt_sub_index_ids_orig[1:5,1:5])
        if("average" %in% input$sel_benchmark) { # add an average index as a benchmark
cat("  average benchmark\n")          
          # make an average EV table
          ev_avg <- val$dt_ev_filtered[1,,drop = F]
          idx <- which(names(val$dt_ev_filtered) %in% 
                         val$dt_desc_ev_clean$column_labelling[val$dt_desc_ev_clean$classifier=="EV"])
          ev_mean <- apply(val$dt_ev_filtered[,idx], 2, mean, na.rm = T)
          ev_avg[1,idx] <- ev_mean
          ev_avg[1,"Index"] <- "avg_index"

          dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session, 
                                val$dt_ebv_filtered, ev_avg, val$dt_description_clean, val$dt_desc_ev_clean)
cat("  dt_sub_ebv_index_ids:");print(dim(dt_sub_ebv_index_ids));print(head(dt_sub_ebv_index_ids))
          dt_sub_index_ids <- # ID sex avg_index
            dt_sub_ebv_index_ids[,!names(dt_sub_ebv_index_ids) 
                                 %in% val$dt_description_clean$column_labelling[
                                   val$dt_description_clean$classifier=="EBV"] ]
cat("  dt_sub_index_ids:");print(dim(dt_sub_index_ids));print(head(dt_sub_index_ids))
          # ID sex avg_index, index_1 ... index_3000
          val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig)
cat("  val$dt_sub_index_ids:");print(dim(val$dt_sub_index_ids));print(val$dt_sub_index_ids[1:5,1:10])
          # avg_index, index_1 ... index_3000
          dt_index_avg <- dplyr::select(
            val$dt_sub_index_ids, 
            dplyr::any_of(c("avg_index", val$dt_ev_filtered$Index)))
cat("  dt_index_avg:");print(dim(dt_index_avg));print(dt_index_avg[1:3,1:3])
          by_ave <- makeLongCor(input, output, session,
                                cor(dt_index_avg, use = "pairwise.complete.obs"),
                                reactive("avg_index")) # Index aggregated_index correlation ID
          by_cluster <- rbind(by_cluster, by_ave)
cat("  average index by_cluster:\n");print(tail(by_cluster))
        } else if("upload" %in% input$sel_benchmark) { # add uploaded indexes as benchmarks
# cat("  upload in sel_benchmark\n   val$dt_bnchmrk_ev_cleaned:\n")
print(head(val$dt_bnchmrk_ev_cleaned))
          if(is.null(val$dt_bnchmrk_ev_cleaned)) return(by_cluster)
# cat("  req satsified\n")          
          dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session, 
                                val$dt_ebv_filtered, val$dt_bnchmrk_ev_cleaned, 
                                val$dt_description_clean, val$dt_desc_ev_clean)
          
          dt_sub_index_ids <- # ID sex bnchmrk_index ...
            dt_sub_ebv_index_ids[,!names(dt_sub_ebv_index_ids) 
                                 %in% val$dt_description_clean$column_labelling[
                                   val$dt_description_clean$classifier=="EBV"] ]
# cat("  dt_sub_index_ids:");print(dim(dt_sub_index_ids));print(head(dt_sub_index_ids))
          # ID sex bnchmrk_index, index_1 ... index_3000
          val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig)
# cat("  dt_sub_index_ids:");print(dim(val$dt_sub_index_ids));print(val$dt_sub_index_ids[3:5,3:5])
          # animal ID x Index
          dt_index_bnchmrk <- dplyr::select(
            val$dt_sub_index_ids, 
            dplyr::any_of(c(val$dt_bnchmrk_ev_cleaned$Index, val$dt_ev_filtered$Index)))
# cat("  dt_index_bnchmrk dim: ");print(dim(dt_index_bnchmrk));print(dt_index_bnchmrk[3:5,3:5])
          # bnchmrk_index, index_1 ... index_3000
          by_bnchmrk <- makeLongCor(input, output, session,
                                    cor(dt_index_bnchmrk, use = "pairwise.complete.obs"),
                                    reactive(val$dt_bnchmrk_ev_cleaned$Index))
          by_cluster <- rbind(by_cluster, by_bnchmrk) # Index aggregated_index correlation ID
# cat("  upload computed\n   by_cluster:");print(head(by_cluster))     
        } # if "upload" in input$sel_benchmark
        
        return(by_cluster)
      }) # cor_default eventReactive
      
      # Heatmap or scatter plot
      output$plot_cor_default <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(cor_default, input$font_size)
# cat(" renderPlot plot_cor_default\n")
# cat("  cor_default:");print(head(cor_default()))
        # initial parameters
        width  <- session$clientData[[paste0("output_", session$ns("plot_cor_default"), 
                                             "_width")]]
       # cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256)
        
        # whent there are too many indexes, draw a bar chart instead of a heatmap
        # if(length(tempVar$corr) < 11) { # doesn't print to UI...
        #   heat_map <- list(p = pheatmap::pheatmap(tempVar$corr, 
        #                                  color = rev(cols), 
        #                                  # breaks = seq(-1, 1, length.out = 256),
        #                                  cluster_cols = F, cluster_rows = F, 
        #                                  fontsize = input$font_size),
        #                    df = tempVar$corr)
        #   
        # } else {
        # list(p, df). df cols are Index aggregated_index correlation id
        heat_map <- plotcorrDot(input, output, session,
                                cor_default(), font_size = reactive(input$font_size))
        # }
        
        downloadPlotModuleServer("dnld_heat_default", 
                                 name = "heatmap_new_and_original_indexes",
                                 plots = heat_map$p, # if(class(heat_map)[1]=="list") {
                                 #   gridExtra::grid.arrange(grobs = heat_map, ncol = 1)} else {heat_map}, 
                                 width = reactive(width)
        )
        
       # tempVar$corr_df <- heat_map$df
        
        downloadModuleServer("dnld_cor_default", "correlation_new_and_original_indexes",
                             heat_map$df, T)
        
        return( heat_map$p
                # if(class(heat_map)[1]=="list") {
                # gridExtra::grid.arrange(grobs = heat_map, ncol = 1)} else {heat_map}
        )
      }) }) #, height = 800) # can't use reactive values for height
      
      # CALCULATE SELECTED CORRELATION
      observeEvent(input$run_heatmap,{
# cat(" observe run_heatmap val$dt_index_new");print(dim(val$dt_index_new));#print(head(val$dt_index_new))
# cat("  sel_agg:");print(input$sel_agg);cat("  sel_index:", input$sel_index, " sel_cluster:",
# input$sel_cluster, " dt_ev_agg: ");print(dim(dt_ev_agg()))
# cat("  clusters:", class(clusters()), length(clusters()), " ncol dt_index_new:", ncol(val$dt_index_new),
    # " len dt_ev_agg$Index:", length(dt_ev_agg()$Index), "\n")
        req(input$sel_agg!="", # input$sel_top_n,
            ncol(val$dt_index_new)>=length(clusters())+length(dt_ev_agg()$Index))

        sel_index <- sapply(strsplit(input$sel_index, "\\{"), head, 1) %>% unlist()
        
        # find indexes that belong to the selected cluster
        if(input$sel_cluster[1] %in% clusters()) { # clustering result
          sel_cluster <- names(clusters())[grep(input$sel_cluster, clusters())]
          
        } else if (input$sel_cluster[1]=="" ) {    # not selected
          sel_cluster <- "#"
          
        } else {                                   # user predefined group
          group_headers <- val$dt_desc_ev_clean$column_labelling[
            val$dt_desc_ev_clean$classifier=="Group"]
          
          group_levels <- sapply(val$dt_ev_filtered[,group_headers,drop = F], unique, simplify = F)
          
          if(length(group_headers) > 1) {
            group_header <- group_headers[sapply(group_levels, match, input$sel_cluster[1]) %>% 
                                            sapply(sum, na.rm = T) > 0] # should only have 1 TRUE
          } else {
            group_header <- group_headers
          }
          
          sel_cluster <- val$dt_ev_filtered$Index[which(val$dt_ev_filtered[, group_header]
                                                        ==input$sel_cluster)]
        }
        
        tempVar$sel_index <- sel_index
        tempVar$sel_cluster <- sel_cluster
# cat("  sel_index:");print(sel_index);cat("  sel_cluster:");print(sel_cluster)
        if(sel_cluster[1]!="#") {
          df_index_sub <- dplyr::select(val$dt_index_new, 
                                        dplyr::any_of(c(input$sel_agg, sel_cluster)))
          
        } else {
          df_index_sub <- dplyr::select(val$dt_index_new, 
                                        dplyr::any_of(c(input$sel_agg, sel_index)))
        }
# cat("  df_index_sub:");print(dim(df_index_sub));# print(head(df_index_sub))
        tempVar$df_for_dx2 <- df_index_sub
        
        # correlation
        if(ncol(df_index_sub) >= 2) { # only calculate corrlations when >=2 vars are selected
          
          tempVar$corr <- cor(df_index_sub, use = "pairwise.complete.obs")
        } # if ncol df_index_sub >=2
        
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
      }) # observe run_heatmap
      
      # Heatmap or scatter plot
      output$plot_cor <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(length(tempVar$corr) > 1, input$font_size)
# cat(" renderPlot plot_cor\n")
        # initial parameters
        width  <- session$clientData[[paste0("output_", session$ns("plot_cor"), 
                                                     "_width")]]
        m <- makeLongCor(input, output, session,
                         tempVar$corr, reactive(input$sel_agg))
        heat_map <- plotcorrDot(input, output, session,
                                m, font_size = reactive(input$font_size))
        # }
        
        downloadPlotModuleServer("dnld_heat",
          name = paste0("heatmap_", 
                        paste0(input$sel_agg, collapse = "_"),
                        "_", tempVar$sel_index,
                        "_", input$sel_cluster),
          plots = heat_map$p,  width = reactive(width))
        
        tempVar$corr_df <- heat_map$df
        
        downloadModuleServer("dnld_cor", 
                             paste0("correlation_", paste0(input$sel_agg, collapse = "_"), "_",
                                    tempVar$sel_index, "_",
                                    input$sel_cluster),
                             tempVar$corr, T)
        
        return( heat_map$p)
      }) }) #, height = 800) # can't use reactive values for height
      
      # Correlation by quantile table
      q_table <- #eventReactive(input$quantile | input$run_heatmap, {
        reactive({
# cat(" eventReactive quantile\n input$quantile: ", input$quantile, "tempVar$corr_df:\n")
# print(head(tempVar$corr_df))
        req("id" %in% colnames(tempVar$corr_df), input$quantile)  # Index aggregated_index correlation id

        n <- round(max(tempVar$corr_df$id)*input$quantile/100, 0)
        out <- dplyr::filter(tempVar$corr_df, id == n) %>% 
          dplyr::select(-Index)
        names(out)[which(names(out)=="id")] <- "n_indexes"
        
        return(out)
      })
      
      renderDtTableModuleServer("quantile_table", q_table, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = paste0("min_corr_at_", input$quantile, "%"))
      
      return(reactive(tempVar$df_for_dx2))
    })}


aggDxModSidebarUI2 <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Main"),
    wellPanel(
      # selectInput(ns("sel_agg"), "Select an aggregated index", choice = "", multiple = T),
      # selectInput(ns("sel_index"), "Select an original index", choices = ""),
      # selectInput(ns("sel_cluster"), "Or select all indexes from a cluster", choices = ""),
      shinyjs::hidden(
        div(id = ns("top"),
            # tags$table(
            #  tags$td(
            sliderInput(ns("sel_top_n"), "Select top # individuals", 1, 10, 10, 1),
            # tags$td(
            checkboxInput(ns("percent"), "Use percentage", F, width = "30%")#)
            #)
        )),
      actionButton(ns("run_top"), "Run analysis", icon("running"), 
                   class = "btn btn-primary")
    ),
    h4("Plot control"),
    wellPanel(
      # numericInput(ns("show_n_indexes"), "# indexs to show", 10, 1, 10, 1),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
    )
  )
}

aggDxModUI2 <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    span(textOutput(ns("error_m")), class = "text-danger"),
    h1("Top individual(s)"),
    h2(textOutput(ns("top_n_title"))),
    h3("Table"),
    renderDtTableModuleUI(ns("top_n_index")),
    br(),br(),
    h3("Scatter plot"),
    plotOutput(ns("plot_top_n")),# height = "800px"),
    downloadPlotModuleUI(ns("dnld_plot_top"))
  )
}

#' Diagnosis tools 2 for aggregated indexes
#'
#' @description Show the top individual overlap/agreement across indexes and aggregated indexes
#' @param id shiny object id
#' @param dt_index_sub a reactive function of a data.frame from aggDxMod
#' @param sel_index a reactive function. The input$sel_index from aggDxMod
#' @return 
aggDxMod2 <- function(id, transpose = F, 
                      dt_index_sub = reactive(NULL), dt_index = reactive(NULL),
                      sel_index = reactive(""), sel_agg = reactive(""), sel_cluster = reactive(""),
                      ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("aggDxMod2\n")
      # INITIALIZE
      tempVar <- reactiveValues()
      
      output$error_m <- renderText({
        # cat(" erro_m:");print(input$error_m) # always NULL
        # cat("  names val:");print(names(val))
        validate(need(!is.null(dt_index_sub()), "Please finish step 2"),
                 need(!is.null(dt_index()), "Please finish step 2"),
                 need(sel_agg()[1]!="", "Please select at least 1 aggregated index at step 2"),
                 need(any(sel_index()!="", sel_cluster()!=""), 
                      "Please select an index or a cluster of indexes at step 2")
        )
      })
      
      observeEvent(!is.null(dt_index()), {
# cat("  ", class(dt_index()));print(dim(dt_index()))
       req(!is.null(dt_index()))
# cat("  req met\n")
# cat("  sel_agg:");print(sel_agg());cat("  sel_index:", sel_index(), " sel_cluster:", sel_cluster(),
# " sel_top_n:", input$sel_top_n, " percent:", input$percent, "\n")
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
      
      observeEvent(isTRUE(sel_index()!="" || sel_cluster()!=""), { # react even when False!
# cat(" observe sel_index or sel_cluster not empty\n")
        # updateNumericInput(session, "show_n_indexes",
        #                    max = max(length(sel_index()),length(sel_cluster())))
        if(sel_index()=="" && sel_cluster() == "") {
          shinyjs::hide("top")
        } else {
          shinyjs::show("top")
        }
      }, ignoreInit = T)
      
      # CALCULATE
      observeEvent(input$run_top,{
# cat(" observe run_top dt_index_sub");print(dim(dt_index_sub()));print(head(dt_index_sub()))
# cat("  sel_agg:");print(sel_agg());cat("  sel_index:", sel_index(), " sel_cluster:",
# sel_cluster(), " sel_top_n:", input$sel_top_n, " percent:", input$percent, "\n")
        req(input$sel_top_n, # input$percent, # doesn't work if ==F
          !is.null(dt_index_sub()))
# req(" req satisfied\n")        
        df_index_sub <- dt_index_sub()
        
        # find n
        if(!input$percent) {
          n <- input$sel_top_n
        } else {
          n <- min(round(nrow(df_index_sub)*input$sel_top_n/100, 0), nrow(df_index_sub))
        }
        tempVar$n <- n
        # cat("  n: ", n, "\n")
        
        output$top_n_title <- renderText({
          req(n)
          
          return(paste0("Top ", input$sel_top_n, ifelse(input$percent, "%", ""),
                        " individual agreement among indexes"))
        })
        
        # ranking agreement
        df_top_n <- lapply(names(df_index_sub), function( i ){ # by index
          
          idx <- order(df_index_sub[,i], decreasing = T)
          out <- data.frame(order = 1:length(idx), Index = rep(i, length(idx)),
                            plant = rownames(df_index_sub)[idx], value = df_index_sub[idx,i], 
                            check.names = F)
          return(head(out, n))
        })
        names(df_top_n) <- names(df_index_sub)
        tempVar$df_top_n <- df_top_n
        # cat("  df_top_n:", class(df_top_n));print(dim(df_index_sub));print(tempVar$df_top_n[[1]][1,])
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
      }) # observe input$run_heatmap
      
      # TOP N individual agreement plot
      output$plot_top_n <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(tempVar$df_top_n, input$font_size)
        
        # initial parameters
        width_top  <- session$clientData[[paste0("output_", session$ns("plot_top_n"), 
                                                 "_width")]]
        
        l <- plotTopNdot(input, output, session,
                         tempVar$df_top_n, reactive(sel_agg()),
                         reactive(input$font_size))
        
        tempVar$top_n_df <- l$df # Index   aggregated_index    n   percent id
        
        downloadPlotModuleServer(
          "dnld_plot_top", 
          name = paste0("top_", tempVar$n, "_", paste0(sel_agg(), collapse = "_"), "_", 
                        tempVar$sel_index, "_", sel_cluster()),
          plots = l$p, # if(class(plot_top_n)[1]=="list") {
          # gridExtra::grid.arrange(grobs = plot_top_n, ncol = 1)} else {plot_top_n},
          width = reactive(width_top))
        
        # if("ggplot2" %in% class(plot_top_n)) {
        return(l$p)
        
        # } else if(class(plot_top_n)[1]=="list") { #if (length(plot_top_n)==1) {
        # return(gridExtra::grid.arrange(grobs = plot_top_n, ncol = 1))
        # }
      }) })#, height = 800)]
})} # aggDXMod2
        
        
aggDxModSidebarUI3 <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Main"),
    wellPanel(
      selectInput(ns("class_group_var"), "Choose a classification variable:", "", "") 
    ),
    htmltools::HTML(strrep(br(), 35)),
    h4("Plot control"),
    wellPanel(
      selectInput(ns("agg_by"), "Select an index type", "", ""),
      checkboxInput(ns("use_count"), "Use count", F),
      # checkboxInput(ns("switch_index_classvar"), 
                    # "Switch between indexes and classification variables", F) 
    )
  )
}

aggDxModUI3 <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Aggregated index diagnosis - more"),
    textOutput(ns("error_m")),
    h2("Classification variable distribution"),
    renderDtTableModuleUI(ns("classvar_summary")),
    br(),br(),
    h2("Bar chart"),
    plotOutput(ns("classvar_plot"), height = "800px"),
    downloadPlotModuleUI(ns("dnld_cv_plot"))
  )
}

#' Diagnosis tools 3 for aggregated indexes
#'
#' @description Show the classVar and Group pattern of different aggregated indexes. 
#'              The classVar and Group come from the EV file
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{reactive(
#'        val$dt_index_new)}, a data.frame of animal by index. If the data is index by animal, 
#'        then \code{transpose} should be set to \code{T}.
#'        2) \code{clusters}, a reactive function of a cluster assignment vector. e.g. a 
#'        \code{cutree} output \code{val$cl$clusters}. 4) \code{val$dt_desc_ev_clean}, a data.frame
#'        of 2 columns: column_labelling and classifier/ 5) \code{val$dt_ev_filtered}, a data.frame
#'         of columns Index, classVar (optional) and trait names
#' 
#' @return a table and a plot
aggDxMod3 <- function(id, val, transpose = F, clusters = reactive(NULL), 
                     ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("aggDxMod3\n")      
      # INITIALIZE
      tempVar <- reactiveValues()
      
      output$error_m <- renderText({
# cat(" erro_m:");print(input$error_m) # always NULL
# cat("  names val:");print(names(val))
        validate(need(!is.null(val$dt_desc_ev_clean), "Please upload an EV description file"),
                 need(!is.null(val$dt_ev_filtered), "Please upload an EV file"),
                 # need(!is.null(val$dt_index_new), "Please finish filtering or upload an index table"),
                 need(!is.null(clusters()), "Please upload a cluster table"),
                 #     "Please finish 'run cluster' or upload a cluster table"),
                 #need(!is.null(dt_ev_agg()), "Please finish 'Make new weights'"),
                 need(input$class_group_var!="",  "Please select a classification variable")
        )
      })
      
      # update input$class_group_var
      observeEvent(!is.null(clusters()), { 
# cat(" observeEvent clusters:");print(head(clusters()));print(dim(val$dt_desc_ev_clean))
# print(val$dt_desc_ev_clean)
        req(!is.null(val$dt_desc_ev_clean))

        class_group_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier %in% c("ClassVar", "Group")]
# cat("  class_vars: ");print(class_vars)
        updateSelectInput(session, "class_group_var", choices = c("", class_group_vars))
      }, ignoreInit = T)
      
      classvar_summary <- eventReactive(input$class_group_var, {
        req(clusters, val$dt_ev_filtered, val$dt_desc_ev_clean, input$class_group_var!="")
# cat(" eventreactive input$class_group_var\n  input$class_group_var:", input$class_group_var, "\n")
        class_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier == "ClassVar"]

        group_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier == "Group"]
        group_vars <- c(group_vars, "new_index_by_cluster")
        
        # Merge ClassVar and new index
        # Index RM State... Group1... new_index_by_cluster 
        df_cluster <- data.frame(Index = names(clusters()), new_index_by_cluster = clusters(),
                                 check.names = F)
        df_index_classvar_group <- dplyr::select(
          val$dt_ev_filtered, Index, matches(all_of(class_vars)), matches(all_of(group_vars))) %>% 
          right_join(df_cluster, by = "Index")
# write.table(df_index_classvar_group, "df_index_classvar_group.txt", quote = F, row.names = F, sep = ",")
        # calculate summary stat table
        # aggregated_by, aggregated_index, n, percent
  
        df_summary_table <- do.call(rbind, lapply(group_vars, function(group_var) {
cat(" group_var:", group_var, "\n")
print(group_by(df_index_classvar_group, 
               across(unique(c(group_var, input$class_group_var)))) %>% 
        tally() %>% 
        group_by(.data[[input$class_group_var]]) %>% head())
          out <- group_by(df_index_classvar_group, 
                          across(unique(c(group_var, input$class_group_var)))) %>% 
            tally() %>% 
            group_by(.data[[input$class_group_var]]) %>% 
            mutate(count = sum(n), percent = n/count) %>% dplyr::select(-count)
          names(out)[1] <- "aggregated_index"
          
          return(data.frame(aggregated_by = group_var, out))
        }))
        
        # select input aggregated_by
        updateSelectInput(session, "agg_by", choices = c("", df_summary_table$aggregated_by))
        
        return(df_summary_table)
      })
        
      renderDtTableModuleServer("classvar_summary", classvar_summary, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = "class_var_summary_in_agg_index")
      
      # # select input aggregated_by
      # observeEvent(length(df_summary_table()) > 0, {
      #   updateSelectInput(session, "agg_by", choices = c("", df_summary_table()$aggregated_by))
      # })
      
      # draw percentage plot
      output$classvar_plot <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
# cat(" classvar_plot\n class_var:", input$class_group_var, "\n"); print(classvar_summary()[1,])
        req(input$class_group_var!="", input$agg_by!="")

        width <- session$clientData[[paste0("output_", session$ns("classvar_plot"), "_width")]]
        df <- classvar_summary() %>% 
          dplyr::filter(aggregated_by == input$agg_by)
# cat("  df:", class(df), "\n");print(sapply(df, class))
        p <- plotClassvarBar(df, input$class_group_var, "aggregated_index", input$use_count)
# cat("  p:", class(p),"\n");#print(p)
        downloadPlotModuleServer(
          "dnld_cv_plot", "classvar_by_index", p,
          # gridExtra::grid.arrange(grobs = ps,
                                  # nrow = min(2, length(unique(df$aggregated_index)))),
          reactive(width))
        
        return(p) #gridExtra::grid.arrange(grobs = ps,
                                       # nrow = min(2, length(unique(df$aggregated_index)))))
      }) })
    })}