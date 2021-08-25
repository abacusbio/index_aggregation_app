aggDxModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional)"),
    wellPanel(
      uploadTableModuleUI(ns("upload_benchmark"), "Benchmark index EV file (optional)"),
      div(textOutput(ns("error_m_2")), class = "text-warning")
    ),
    h4("All index"),
    wellPanel(
      selectInput(ns("sel_benchmark"), "Select a benchmark index", 
                  choice = c("", "average"))
    ),
    h4("Plot display control"),
    wellPanel(
      # numericInput(ns("show_n_indexes"), "# indexs to show", 10, 1, 10, 1),
      checkboxInput(ns("fixed_y_scale"), "Y scale fixed 0-1", T),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
      ),
    htmltools::HTML(strrep(br(), 27)),
    h4("User select indexes"),
    wellPanel(
      selectInput(ns("sel_agg"), "Select an aggregated index", choice = "", multiple = T),
      selectInput(ns("sel_index"), "Select an original index", choices = ""),
      selectInput(ns("sel_cluster"), "Or select all indexes from a cluster", choices = ""),
      actionButton(ns("run_analysis"), "Run analysis", icon("running"), 
                   class = "btn btn-primary")
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
    span(textOutput(ns("error_m")), class = "text-warning"), # input file missing
    span(textOutput(ns("warn_m")), class = "text-warning"), # input file format wrong
    plotOutput(ns("plot_cor_default")),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_heat_default"))),
      tags$td(downloadModuleUI(ns("dnld_cor_default"), "Download the correlation"))
    ),
    br(),br(),
    h3("Minimum correlation given a quantile"),
    numericInput(ns("quantile_default"), "Enter a quantile", 100, 1, 100, 1),
    renderDtTableModuleUI(ns("quantile_table_default")),
    downloadModuleUI(ns("poor_cor_index"), "Download poorly correlated index table"),
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
    numericInput(ns("quantile"), "Enter a quantile", 100, 1, 100, 1),
    renderDtTableModuleUI(ns("quantile_table")),
    downloadModuleUI(ns("poor_cor_index_sub"), "Download poorly correlated index table")
  )
}

#' Diagnosis tools for aggregated indexes
#'
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{reactive(
#'        val$dt_index)}, a data.frame of animal by index. If the data is index by animal, 
#'        then \code{transpose} should be set to \code{T},
#'        2) \code{cl}, a list containing \code{cl_obj}, a class "hclust" or "agnes" object,
#'        and \code{val$cl$clusters}, a cluster assignment vector. e.g. a \code{cutree} 
#'        output, 3)\code{val$dt_ev_avg}, a data.frame of the same structure as \code{dt_ev_agg}, 4)
#'         \code{val$dt_desc_ev_clean}, a data.frame of 2 columns: column_labelling and 
#'        classifier, 5) \code{val$dt_ev_filtered}, a data.frame of columns Index, ClassVar
#'        (optional) and trait names, 6)\code{val$dt_description_clean}, a data.frame of columns
#'        column_labelling and classifier
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
        validate(need(!is.null(val$dt_description_clean), 
                      "Please upload an EBV description file in 'Make new weights'"),
                 need(!is.null(val$dt_desc_ev_clean), 
                      "Please upload an EV description file in 'Make new weights'"),
                 need(!is.null(val$dt_ebv_filtered), 
                      "Please upload an EBV file in 'Make new weights'"),
                 need(!is.null(val$dt_ev_filtered), 
                      "Please upload an EV file in 'Make new weights'"),
                 need(!is.null(val$dt_index), 
                      "Please finish filtering or upload an index table in 'Make new weights'"),
                 need(!is.null(clusters()), #val$cl$clusters),
                     "Please finish 'run cluster' or upload a cluster table in 'Make new weights'"),
                 need(!is.null(dt_ev_agg()), "Please finish 'Make new weights'"),
                 need(!is.null(val$dt_ev_avg), "Please finish 'Make new weights'")
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
# cat(" clusters:\n");print(head(clusters()));cat(" val$dt_ev_filtered:\n");print(head(val$dt_ev_filtered))
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
      observeEvent( #input$run_analysis,{
        !is.null(dt_ev_agg()) && !is.null(val$dt_ebv_filtered) && 
          !is.null(val$dt_description_clean) && is.null(val$dt_ev_avg),
        {
# cat(" observe 3 data\n")#, names val: ");print(names(val))
# cat("  dt_ev_agg:");print(dim(dt_ev_agg()));print(dt_ev_agg())
# cat("  ebv_filtered:");print(dim(val$dt_ebv_filtered))
# cat("  description_clean:");print(dim(val$dt_description_clean))
# cat("  desc_ev_clean:");print(dim(val$dt_desc_ev_clean))
# cat("  dt_index:");print(dim(val$dt_index));#print(val$dt_index[1:3,1:3]);print(names(val$dt_index))
          req(!is.null(dt_ev_agg()), !is.null(val$dt_ebv_filtered), !is.null(clusters()),
              !is.null(val$dt_description_clean),
              !is.null(val$dt_desc_ev_clean), !is.null(dt_index()))
          
          # avoid recalculation
          if(sum(is.na(match(dt_ev_agg()$Index, colnames(val$dt_index_new)))) > 0) {
            
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
# cat("  val$dt_index_new dim: ");print(dim(val$dt_index_new));print(val$dt_index_new[3:5,1:5]) #dt_ev_agg()$Index])
            rownames(val$dt_index_new) <- val$dt_sub_index_ids$ID
          } # if new_index? not in val$dt_index_new
        }, ignoreInit = T) # observe 3 datasets
      
      # CALCULATE DEFAULT CORRELATION
      cor_default <- eventReactive(
        {nrow(val$dt_index_new)
          input$sel_benchmark}, 
        {
# cat(" event reactive val$dt_index_new\n")
# cat("  dt_index_new:");print(dim(val$dt_index_new));print(val$dt_index_new[1:3, 1:5]) #dt_ev_agg()$Index])
        req(!is.null(clusters()), !is.null(dt_ev_agg()), !is.null(val$dt_index_new))
# cat("  clusters unique:");print(unique(clusters()))        
        by_cluster <- do.call(rbind, lapply(unique(clusters()), function(i) {
          
          sel_agg <- grep(paste0("_", i, "$"), dt_ev_agg()$Index, value = T)
          sel_cluster <- names(clusters())[which(clusters()==i)]
          df_index_sub <- dplyr::select(val$dt_index_new, 
                                        dplyr::any_of(c(sel_agg, sel_cluster)))
          m <- cor(df_index_sub, use = "pairwise.complete.obs")
# cat("   i:", i, "sel_agg:", paste0(sel_agg), "m dim:");print(dim(m))
          out <- makeLongCor(input, output, session,
                             m, reactive(sel_agg)) # Index aggregated_index correlation id
          return(out)
        }))
        
        by_cluster <- dplyr::arrange(by_cluster, desc(correlation)) %>% 
          mutate(id = dplyr::row_number())  # sort id by correlation
                 # index_type = "aggregation") # Index aggregated_index correlation id
# cat("  val$dt_index_new:");print(val$dt_index_new[1:3,1:3])        
        index <- dplyr::select(val$dt_index_new, !matches("new_index_"))
        dt_sub_index_ids_orig <- data.frame(ID = rownames(index), index, check.names = F)
# cat("  dt_sub_index_ids_orig:");print(dim(dt_sub_index_ids_orig));print(dt_sub_index_ids_orig[1:5,1:5])
        if("average" %in% input$sel_benchmark) { # add an average index as a benchmark
# cat("  average benchmark\n")          
          # make an average EV table
          ev_avg <- val$dt_ev_filtered[1,,drop = F]
          idx <- which(names(val$dt_ev_filtered) %in%
                         val$dt_desc_ev_clean$column_labelling[val$dt_desc_ev_clean$classifier=="EV"])
          ev_mean <- apply(val$dt_ev_filtered[,idx], 2, mean, na.rm = T)
          ev_avg[1,idx] <- ev_mean
          ev_avg[1,"Index"] <- "avg_index"
# cat("  ev_avg old:");print(ev_avg)
          idx_2 <- match(names(ev_avg), names(val$dt_ev_avg))
          idx_1 <- which(!is.na(idx_2))
          ev_avg[,idx_1] <- val$dt_ev_avg[,na.omit(idx_2)]
# cat("  ev_avg new:");print(ev_avg)
          dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session, 
                                val$dt_ebv_filtered, ev_avg, val$dt_description_clean, val$dt_desc_ev_clean)
# cat("  dt_sub_ebv_index_ids:");print(dim(dt_sub_ebv_index_ids));print(head(dt_sub_ebv_index_ids))
          dt_sub_index_ids <- # ID sex avg_index
            dt_sub_ebv_index_ids[,!names(dt_sub_ebv_index_ids) 
                                 %in% val$dt_description_clean$column_labelling[
                                   val$dt_description_clean$classifier=="EBV"] ]
# cat("  dt_sub_index_ids:");print(dim(dt_sub_index_ids));print(head(dt_sub_index_ids))
          # ID sex avg_index, index_1 ... index_3000
          val$dt_sub_index_ids <- left_join(dt_sub_index_ids, dt_sub_index_ids_orig)
# cat("  val$dt_sub_index_ids:");print(dim(val$dt_sub_index_ids));print(val$dt_sub_index_ids[1:5,1:10])
          # avg_index, index_1 ... index_3000
          dt_index_avg <- dplyr::select(
            val$dt_sub_index_ids, 
            dplyr::any_of(c("avg_index", val$dt_ev_filtered$Index)))
         # write.table(dt_index_avg, "../test outputs/half_indexes/dt_index_avg", quote = F, row.names = T, col.names = T)
# cat("  dt_index_avg:");print(dim(dt_index_avg));print(dt_index_avg[1:3,1:3])
          by_ave <- makeLongCor(input, output, session,
                                cor(dt_index_avg, use = "pairwise.complete.obs"),
                                reactive("avg_index")) # Index aggregated_index correlation ID
          by_cluster <- rbind(by_cluster, by_ave)
# cat("  average index by_cluster:\n");print(tail(by_cluster))
        } else if("upload" %in% input$sel_benchmark) { # add uploaded indexes as benchmarks
# cat("  upload in sel_benchmark\n   val$dt_bnchmrk_ev_cleaned:\n")
# print(head(val$dt_bnchmrk_ev_cleaned))
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
          by_cluster <- rbind(by_cluster, by_bnchmrk) # Index aggregated_index correlation id
# cat("  upload computed\n   by_cluster:");print(head(by_cluster))     
        } # if "upload" in input$sel_benchmark
        
        return(by_cluster)
      }) # cor_default eventReactive
      
      # Heatmap or scatter plot
      output$plot_cor_default <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(cor_default, input$font_size)
                       input$fixed_y_scale # force function to react
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
                                cor_default(), "aggregated_index", reactive(input$font_size),
                                reactive(input$fixed_y_scale), 
                                baseline = ifelse(input$sel_benchmark=="", F, T))
        
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
      
      # Correlation by quantile table
      q_table_default <- eventReactive(
        {input$quantile_default
          cor_default()}, { # Index aggregated_index correlation id
       # reactive({
# cat(" eventReactive quantile default\n input$quantile: ", input$quantile, "tempVar$corr_df:\n")
# print(head(tempVar$corr_df))
          req("id" %in% colnames(cor_default())) #, input$quantile_default)  # Index aggregated_index correlation id
          
          n <- as.integer(round(max(cor_default()$id)*input$quantile_default/100, 0))
          out <- dplyr::filter(cor_default(), id == n) #%>% 
            #dplyr::select(-Index)
          names(out)[which(names(out)=="id")] <- "n_indexes"
          
          poor_cor_indexes <- dplyr::filter(cor_default(), id >=n)
          names(poor_cor_indexes)[which(names(poor_cor_indexes)=="id")] <- "order"
          poor_cor_index <- left_join(poor_cor_indexes, val$dt_ev_filtered, by = "Index")
# cat("  poor_cor_index:");print(dim(tempVar$poor_cor_index));print(head(tempVar$poor_cor_index))
          downloadModuleServer("poor_cor_index",
                               paste0("poorly_corr_index_under_", input$quantile_default, "%"),
                               poor_cor_index)
          
          return(out)
        })
      
      observeEvent({q_table_default()
        input$quantile},
        renderDtTableModuleServer("quantile_table_default", q_table_default, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = paste0("min_corr_at_", input$quantile, "%"))
      )
      
      # CALCULATE SELECTED CORRELATION
      observeEvent(input$run_analysis,{
# cat(" observe run_analysis val$dt_index_new");print(dim(val$dt_index_new));#print(head(val$dt_index_new))
# cat("  sel_agg:");print(input$sel_agg);cat("  sel_index:", input$sel_index, " sel_cluster:",
# input$sel_cluster, " dt_ev_agg: ");print(dim(dt_ev_agg()))
# cat("  clusters:", class(clusters()), length(clusters()), " ncol dt_index_new:", ncol(val$dt_index_new),
    # " len dt_ev_agg$Index:", length(dt_ev_agg()$Index), "\n")
        req(input$sel_agg!="", # input$sel_top_n,
            ncol(val$dt_index_new)>=length(clusters())+length(dt_ev_agg()$Index))

        sel_index <- sapply(strsplit(input$sel_index, "\\{"), head, 1) %>% unlist()
        
        # find indexes that belong to the selected cluster
        if(input$sel_cluster[1] %in% clusters()) { # clustering result
          sel_cluster <- names(clusters())[which(clusters()==input$sel_cluster)]
          
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
      }) # observe run_analysis
      
      # Heatmap or scatter plot
      output$plot_cor <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
        req(length(tempVar$corr) > 1, input$font_size)
                       input$fixed_y_scale # force function to react
# cat(" renderPlot plot_cor\n")
        # initial parameters
        width  <- session$clientData[[paste0("output_", session$ns("plot_cor"), 
                                                     "_width")]]
        m <- makeLongCor(input, output, session,
                         tempVar$corr, reactive(input$sel_agg))
        heat_map <- plotcorrDot(input, output, session,
                                m, "aggregated_index", reactive(input$font_size), 
                                reactive(input$fixed_y_scale))
        
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
      q_table <- #eventReactive(input$quantile | input$run_analysis, {
        reactive({
# cat(" eventReactive quantile\n input$quantile: ", input$quantile, "tempVar$corr_df:\n")
# print(head(tempVar$corr_df))
        req("id" %in% colnames(tempVar$corr_df), input$quantile)  # Index aggregated_index correlation id

        n <- as.integer(round(max(tempVar$corr_df$id)*input$quantile/100, 0))
        out <- dplyr::filter(tempVar$corr_df, id == n)#  %>% 
          # dplyr::select(-Index)
        names(out)[which(names(out)=="id")] <- "n_indexes"
        
        poor_cor_indexes <- dplyr::filter(tempVar$corr_df, id >=n)
        names(poor_cor_indexes)[which(names(poor_cor_indexes)=="id")] <- "order"
       
        poor_cor_index_sub <- left_join(poor_cor_indexes, val$dt_ev_filtered, by = "Index")
        downloadModuleServer("poor_cor_index_sub", 
                             paste0("poorly_corr_index_sub_under_", input$quantile, "%"), 
                             poor_cor_index_sub)
        
        return(out)
      })
      
      observeEvent({q_table()
        input$quantile},
        renderDtTableModuleServer("quantile_table", q_table, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = paste0("min_corr"), # _at_", input$quantile, "%"), # doesn't react to input$quantile change?
                                option_list = list(sDom  = '<"top">lrt<"bottom">ip')) # disable search bar
      )
      
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
    h4("Plot display control"),
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
    span(textOutput(ns("error_m")), class = "text-warning"),
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
          out <- data.frame(order = as.integer(1:length(idx)), Index = rep(i, length(idx)),
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
        table_top_n <- cbind(order = as.integer(1:n), table_top_n)
        # cat("  table_top_n2:");print(dim(df_index_sub));print(head(table_top_n[,]))
        renderDtTableModuleServer("top_n_index", reactive(table_top_n),
                                  downloadName = paste0("top_", n, ifelse(input$percent, "%", ""),
                                                        "_by_index"), 
                                  option_list = list(sDom  = '<"top">lrt<"bottom">ip')) # disable search bar
      }) # observe input$run_top
      
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
        
#' ClassVar pattern in different aggregated indexes        
aggDxModSidebarUI3 <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Main"),
    wellPanel(
      selectInput(ns("class_var"), "Choose a classification variable:", "", "") 
    ),
    htmltools::HTML(strrep(br(), 35)),
    h4("Plot display control"),
    wellPanel(
      selectInput(ns("agg_by"), "Select an index type", "", ""),
      checkboxInput(ns("use_count"), "Use count", F),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
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
    span(textOutput(ns("error_m")), class = "text-warning"),
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
                 need(input$class_var!="",  "Please select a classification variable")
        )
      })
      
      # update input$class_var
      observeEvent(!is.null(clusters()), { 
# cat(" observeEvent clusters:");print(head(clusters()));print(dim(val$dt_desc_ev_clean))
# print(val$dt_desc_ev_clean)
        req(!is.null(val$dt_desc_ev_clean))

        class_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier %in% "ClassVar"]
# cat("  class_vars: ");print(class_vars)
        updateSelectInput(session, "class_var", choices = c("", class_vars))
      }, ignoreInit = T)
      
      # create table for plots
      # aggregated_by aggregated_index [group_var] [level] n percent
      classvar_summary <- eventReactive(input$class_var, {
        req(clusters, val$dt_ev_filtered, val$dt_desc_ev_clean, input$class_var!="")
# cat(" eventreactive input$class_var\n  input$class_var:", input$class_var, "\n")
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
        df_summary_table <- do.call(rbind, lapply(group_vars, function(group_var) {

          out <- group_by(df_index_classvar_group, 
                          across(unique(c(group_var, input$class_var)))) %>% 
            tally(wt = n()) %>% 
            group_by(.data[[input$class_var]]) %>% 
            mutate(count = as.integer(sum(n)), percent = n/count*100) %>% dplyr::select(-count)
          names(out)[1] <- "aggregated_index"
# cat(" df_summary_table:\n");print(head(out))
          return(data.frame(aggregated_by = group_var, out))
        }))
        
        # select input aggregated_by
        updateSelectInput(session, "agg_by", choices = c("", df_summary_table$aggregated_by))
        
        return(df_summary_table)
      }) # classvar_summary
        
      classvar_sum_show <- eventReactive(classvar_summary(), {
        req(!is.null(classvar_summary()), input$class_var)
        
        return(tidyr::pivot_wider(classvar_summary(), 
                           names_from = input$class_var, values_from = c(n, percent)))
      })
      
      observeEvent(classvar_sum_show(),
      renderDtTableModuleServer("classvar_summary", classvar_sum_show, 
                                extensions = c("FixedHeader", "FixedColumns"),
                                downloadName = "class_var_summary_in_agg_index")
      )
      # # select input aggregated_by
      # observeEvent(length(df_summary_table()) > 0, {
      #   updateSelectInput(session, "agg_by", choices = c("", df_summary_table()$aggregated_by))
      # })
      
      # draw percentage plot
      output$classvar_plot <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
# cat(" classvar_plot\n class_var:", input$class_var, "\n"); print(classvar_summary()[1,])
        req(input$class_var!="", input$agg_by!="", input$font_size, classvar_summary)

        width <- session$clientData[[paste0("output_", session$ns("classvar_plot"), "_width")]]
        df <- classvar_summary() %>% 
          dplyr::filter(aggregated_by == input$agg_by)
# cat("  df:", class(df), "\n");print(sapply(df, class))
        p <- plotClassvarBar(# input, output, session,
                             df, input$class_var, "aggregated_index", input$use_count,
                             input$font_size)
# cat("  p:", class(p),"\n");# print(p)
        downloadPlotModuleServer(
          "dnld_cv_plot", "classvar_by_agg_index", p,
          # gridExtra::grid.arrange(grobs = ps,
                                  # nrow = min(2, length(unique(df$aggregated_index)))),
          reactive(width))
        
        return(p) #gridExtra::grid.arrange(grobs = ps,
                                       # nrow = min(2, length(unique(df$aggregated_index)))))
      }) })
    })}

#' weighting pattern in aggregated indexes
aggDxModSidebarUI4 <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Table display control"),
    wellPanel(
      numericInput(ns("digits"), "Number of decimal places", 0, 0, 20, 1)
    ),
    htmltools::HTML(strrep(br(), 35)),
    h4("Plot display control"),
    wellPanel(
      selectInput(ns("weight_var"), "Choose a weighting variable:", "", ""),
      selectInput(ns("agg_by"), "Select an index type", "", ""),
      checkboxInput(ns("use_count"), "Use count", F),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
      # checkboxInput(ns("switch_index_classvar"), 
      # "Switch between indexes and classification variables", F) 
    )
  )
}

aggDxModUI4 <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Aggregated index diagnosis - more"),
    span(textOutput(ns("error_m")), class = "text-warning"),
    h2("Classification variable distribution"),
    renderDtTableModuleUI(ns("weight_summary_show")),
    br(),br(),
    h2("Bar chart"),
    span(textOutput(ns("error_m_plot")), class = "text-warning"),
    plotOutput(ns("weight_plot"), height = "800px"),
    downloadPlotModuleUI(ns("dnld_wt_plot"))
  )
}

#' Diagnosis tools 4 for aggregated indexes
#'
#' @description Show the index weighting pattern of different aggregated indexes. 
#'              The weighting comes from the EV weighting file
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{clusters}, a reactive
#'        function of a cluster assignment vector. e.g. a 
#'        \code{cutree} output \code{val$cl$clusters}. 4) \code{val$dt_desc_ev_clean}, a data.frame
#'        of 2 columns: column_labelling and classifier/ 5) \code{val$dt_ev_filtered}, a data.frame
#'         of columns Index, classVar (optional) and trait names
#' @param dt_ev_agg a reactive object with val$dt_ev_agg in it, which is a data.frame of columns as 
#'        Index, cluster and traits (EVs).
#' @return a table and a plot
aggDxMod4 <- function(id, val, dt_ev_agg = reactive(NULL), dt_w_clean = reactive(NULL), 
                      clusters = reactive(NULL), ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("aggDxMod4\n")      
      # INITIALIZE
      tempVar <- reactiveValues()
      
      output$error_m <- renderText({
# cat(" erro_m:");print(input$error_m) # always NULL
# cat("  names val:");print(names(val)) # all exist
        validate(need(!is.null(val$dt_desc_ev_clean), "Please upload an EV description file"),
                 need(!is.null(val$dt_ev_filtered), "Please upload an EV file"),
                 need(!is.null(dt_ev_agg()), "Please finish Step 1"),
                 need(!is.null(clusters()), "Please upload a cluster table at Step 1"),
                 need(!is.null(dt_w_clean()), "Please upload a EV weight file")
        )
      })
      
      output$error_m_plot <- renderText({
        validate(need(input$weight_var!="",  "Please select a weighting variable"),
                 need(input$agg_by!="", "Please select an aggregation variable"))
      })
      
      # update input$class_var
      observeEvent(dt_w_clean(), { 
        req(!is.null(dt_w_clean()))
        
        weight_cols <- tail(names(dt_w_clean()), -1)
        updateSelectInput(session, "weight_var", 
                          choices = c("", weight_cols)) #, selected = weight_cols[1])
      }, ignoreInit = T)
      
      # create table for plots
      # aggregated_by aggregated_index sum_[weight] # [group_var] [level] n percent
     weight_summary <- eventReactive({ # doesn't work!!!
     # observeEvent({
        dt_ev_agg()
        dt_w_clean()
        clusters()
        input$digits}
        , {
# cat(" eventReactive weight_summary\n  clusters:"); #print(head(clusters()))
# cat("  dt_ev_agg:", class(dt_ev_agg()), "\n");print(head(dt_ev_agg()))
        req(!is.null(clusters()), !is.null(dt_ev_agg()), !is.null(dt_w_clean()))
          
        weight_cols <- tail(names(dt_w_clean()), -1)

        class_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier == "ClassVar"]
        
        group_vars <- val$dt_desc_ev_clean$column_labelling[
          val$dt_desc_ev_clean$classifier == "Group"]
        group_vars <- c(group_vars, "new_index_by_cluster")
# cat("  weight_cols:");print(weight_cols);cat("  class_vars:");print(class_vars)
# cat("  group_vars:");print(group_vars)
        # Merge ClassVar and new index
        df_ev_agg <- dt_ev_agg()
        names(df_ev_agg)[1] <- "aggregated_index"
        
        df_cluster <- data.frame(Index = names(clusters()), 
                                 new_index_by_cluster = clusters(), check.names = F)

        df <- left_join(dt_w_clean(), df_cluster, by = "Index") %>% # Index [weight] cluster
          left_join(df_ev_agg, by = c("new_index_by_cluster"="cluster")) # Index [weight] new_index_by_cluster aggregated_index traits 

        # Index RM State... Group1... new_index_by_cluster [weight] aggregated_index traits 
        df_index_weight_group <- dplyr::select(
          val$dt_ev_filtered, Index, matches(all_of(class_vars)), matches(all_of(group_vars))) %>% # Index RM State... Group1...
          right_join(df_cluster, by = "Index") %>%  # new_index_by_cluster
          right_join(dt_w_clean(), df_cluster, by = "Index") %>% # [weight]
          right_join(df_ev_agg, by = c("new_index_by_cluster"="cluster"))
# cat("  df_index_weight_group:\n");print(head(df_index_weight_group))
# write.table(df_index_classvar_group, "df_index_classvar_group.txt", quote = F, row.names = F, sep = ",")
        # calculate summary stat table
        # aggregated_by aggregated_index sum_[weight] percent_sum_[weight]
        df_summary_table <- do.call(rbind, lapply(group_vars, function(group_var) {
          
          out <- group_by(df_index_weight_group, 
                          across(all_of(group_var))) %>% 
            summarise(across(all_of(weight_cols), ~sum(.x, na.rm = T), .names = "sum_{col}")) %>% 
            mutate(across(starts_with("sum_"), ~./sum(.)*100, .names = "percent_{col}"))
# cat("  group_var:", group_var, "out:\n");print(head(out))
          names(out)[1] <- "aggregated_index"
          
          return(data.frame(aggregated_by = group_var, out))
        }))
        
        # select input aggregated_by
        updateSelectInput(session, "agg_by", choices = c("", df_summary_table$aggregated_by))
        
     #   tempVar$weight_summary <- df_summary_table 
        return(df_summary_table)
      }, ignoreInit = T) # observeEvent weight_summary
      
     
     renderDtTableModuleServer("weight_summary_show", weight_summary, 
                               extensions = c("FixedHeader", "FixedColumns"),
                               downloadName = "weighting_summary_in_agg_index",
                               digits = reactive(input$digits))
     
      # select input aggregated_by
      observeEvent(length(weight_summary())>0, { # tempVar$weight_summary) > 0, {
        updateSelectInput(session, "agg_by", choices = c("", weight_summary()$aggregated_by)) # tempVar$weight_summary$aggregated_by))
      })
      
      # draw percentage plot
      output$weight_plot <- renderPlot({
        withProgress(
          message = 'Plotting ...', detail = 'This may take a while...', value = 0, {
# cat(" weight_plot\n weight_var:", input$weight_var, "\n  weight_summary:\n")
# print(tempVar$weight_summary[1,])
        req(input$agg_by!="", input$font_size, input$digits, input$weight_var!="",
            # length(tempVar$weight_summary) >0)
            length(weight_summary()) >0)
                       
        width <- session$clientData[[paste0("output_", session$ns("weight_plot"), "_width")]]
        df <- weight_summary() %>% # tempVar$weight_summary %>% 
            dplyr::filter(aggregated_by == input$agg_by)
# cat("  df:", class(df), "\n");print(sapply(df, class))
        y <- ifelse(input$use_count, paste0("sum_", input$weight_var), 
                    paste0("percent_sum_", input$weight_var))
        p <- plotNumvarBar(
          df, x = "aggregated_index", y = y, fill = "aggregated_index",
          input$use_count, input$font_size, input$digits)

        downloadPlotModuleServer("dnld_wt_plot", "weight_by_agg_index", p,
            # gridExtra::grid.arrange(grobs = ps,
            # nrow = min(2, length(unique(df$aggregated_index)))),
            reactive(width))
                       
            return(p) #gridExtra::grid.arrange(grobs = ps,
            # nrow = min(2, length(unique(df$aggregated_index)))))
        }) })
      
    })}