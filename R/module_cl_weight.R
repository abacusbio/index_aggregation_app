calWeiModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional, if you skipped previous steps)"),
    wellPanel(
      uploadTableModuleUI(ns("upload_ebv_desc"), "EBV description file"),
      div(textOutput(ns("error_m_6")), class = "text-warning"),
      uploadTableModuleUI(ns("upload_ebv"), "EBV file"),
      div(textOutput(ns("error_m_7")), class = "text-warning"),
      uploadTableModuleUI(ns("upload_ev_desc"), "EV description file"),
      span(textOutput(ns("error_m_0")), class = "text-danger"),
      uploadTableModuleUI(ns("upload_ev"), "EV file"),
      span(textOutput(ns("error_m_4")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_w"), "EV weight file (optional)"),
      span(textOutput(ns("error_m_5")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_index"), "index table (only has ID and index cols)"),
      span(textOutput(ns("error_m_1")), style = "color:salmon"),
      # uploadTableModuleUI(ns("upload_cl_obj"), "cluster object .RData file"),
      # span(textOutput(ns("error_m_2")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_clusters"), "cluster table"),
      span(textOutput(ns("error_m_3")), style = "color:salmon")
    ),
    h4("Table display control"),
    wellPanel(
      numericInput(ns("view_dec"), "Number of decimal places", 3, 0, 20, 1)
    ),
    htmltools::HTML(strrep(br(), 20)),
    h4("Plot display control"),
    wellPanel(
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1)
      )
  )
}

calWeiModUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h1("Make new index weights"),
    helpText("Based on clusters"),
    div(textOutput(ns("warn_m")), class = "text-warning"),
    div(textOutput(ns("error_m")), class = "text-danger"),
    # shinyjs::hidden(span(id = ns("wait"), p("Calculating...please wait..."), class = "text-danger"")),
    # column(6,
    #     h2("Cluster weights"),
    #     renderDtTableModuleUI(ns("cluster_w"))  
    #   ),
    h2("Index weights"),
    selectInput(ns("choose_w"), "Choose a weight(s)",
                choices = c("equal weight"),# "by index correlation"),
                selected = "equal weight", selectize = T),
    renderRctTableModuleUI(ns("index_w"), "Download the new index weight file"),
    br(),br(),
    h2("New economic valuess"),
    h3("Table"),
    renderTableModuleUI(ns("ew_new"), "Download the new EW file"),
    downloadModuleUI(ns("ew_new_t"), "Download the transposed new EW file"),
    br(),br(),
    h3("Economic value bar Chart"),
    plotOutput(ns("plot_bar_newev")),
    downloadPlotModuleUI(ns("dnld_plot_newev")),
    br(),br(),
    h3("Relative economic value bar chart"),
    plotOutput(ns("plot_bar_newevsd")),
    tags$table(
      tags$td(downloadPlotModuleUI(ns("dnld_plot_newevsd"))),
      tags$td(downloadModuleUI(ns("dnld_newevsd")))
    )
  ) # tagList
}

#' Calculate the weight for each index within a cluster
#'
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{reactive(
#'        val$dt_index)}, a data.frame of animal by index. If the data is index by animal, 
#'        then \code{transpose} should be 
#'        set to \code{T}. 2) \code{cl}, a list containing \code{cl_obj}, a class "hclust" or 
#'        "agnes" object. 3) \code{val$cl$clusters}, a cluster assignment vector. e.g. a \code{cutree} 
#'        output. 4) \code{val$dt_desc_ev_clean}, a data.frame of 2 columns: column_labelling and 
#'        classifier/ 5) \code{val$dt_ev_filtered}, a data.frame of columns Index and trait names
#' @param cl a reactive function of a list containing a cluster_obj and a vector of cluster
#' @param cl a reactive function of a list containing a cluster_obj and a vector of cluster
#'        assignments
#' @param index a reactive function of a data.frame, animal ID by indexes, e.g. val$dt_index        
#' 
#' @return val$dt_weight, a data.frame of 3 columns: Index, cluster and weight; and val$dt_ev_agg,
#'         a data.frame of columns as Index, cluster and traits, where Index are new indexes
calWeiMod <- function(id, val, transpose = T, 
                      val_report, report_prefix = NA, ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("calWeiMod\n")
      # INITIALIZE AND UPLOAD (OPTIONAL)
      tempVar <- reactiveValues(
        cnvrt = data.frame(classifier = c("ID", "ClassVar", "Group", "EBV", "EV"),
                           colClasses = c(rep("character", 3), rep("numeric", 2))))
      
      # observeEvent(input$show_corr, { shinyjs::toggle(id = "bi_clust", condition = !input$show_corr) })
      
      # when user starts the app from this step,
      # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
      ev_desc_user <- uploadTableModuleServer("upload_ev_desc", what = rep("character", 2))
      
      output$error_m_0 <- renderText({
        validate(
          need(class(ev_desc_user())!="try-error", attr(ev_desc_user(), "condition")$message),
          need(names(ev_desc_user())[1]=="column_labelling", 
               "Description file column header wrong."),
          need("EV" %in% ev_desc_user()[,2,drop = T], 
               "Are you sure this is an EV description file?")
        )
      })
      
      observeEvent(length(ev_desc_user()) > 0, { # if use ev_desc_user, only observe once...
        req(class(ev_desc_user())=="data.frame")
        if("dt_desc_ev_clean" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the EV description table by your uploaded file."
          })
        }
        val$dt_desc_ev_clean <- ev_desc_user()
        tempVar$ev_colClasses <- dplyr::left_join(val$dt_desc_ev_clean, tempVar$cnvrt,
                                                  by = "classifier")
      })
      
      index_user <- uploadTableModuleServer("upload_index")
      
      output$error_m_1 <- renderText({
# cat(" error_m_1: names index_user");print(names(index_user())[1]=="ID")
        validate(
          need(class(index_user())!="try-error", attr(index_user(), "condition")$message),
          need(names(index_user())[1]=="ID", "Index file first column should be ID (for plant)")
        )
      })
      
      observeEvent(length(index_user()) > 0, { # if use index_user, only observe once...
# cat(" observe index_user\n  val names:");print(names(val));cat("  error_m_1");print(input$error_m_1)
        req(class(index_user())=="data.frame", names(index_user())[1]=="ID")
        if("dt_index" %in% names(val)) {
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
      #   req(class(cl_obj_user())[1] %in% c("hclust", "agnes"))
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
        req(class(clusters_user())=="data.frame", names(clusters_user())[1]=="Index")
        if(!"cl" %in% names(val)) {
          val$cl <- NULL
          
        } else if("clusters" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the cluster table by your uploaded file."
          })
        }
        
        out <- clusters_user()[,-1,drop = T]
        names(out) <- clusters_user()$Index
# cat(" observe clusters_user\n  out:");print(head(out))
        val$cl$clusters <- out
      })
      
      ew_user <- uploadTableModuleServer("upload_ev", 1, 0, what = tempVar$ev_colClasses$colClasses)
      
      output$error_m_4 <- renderText({
        validate(
          need(class(ew_user())!="try-error", attr(ew_user(), "condition")$message),
          need(names(ew_user())[1]=="Index",
               "EW file headers should be 'Index' and trait names")
        )
      })
      
      observeEvent(length(ew_user())>0, {
        req(class(ew_user())=="data.frame", !is.null(val$dt_desc_ev_clean), 
            names(ew_user())[1]=="Index")
        if("dt_ev_filtered" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the EV table by your uploaded file."
          })
        }
        
        val$dt_ev_filtered <- cleanEVplant(val$dt_desc_ev_clean, ew_user())
      })
      
      w_user <- uploadTableModuleServer("upload_w", 1, 0, what = c("character", "numeric"))
      
      output$error_m_5 <- renderText({
        validate(
          need(class(w_user())!="try-error", attr(w_user(), "condition")$message),
          need(names(w_user())[1]=="Index",
               "EV Weight file headers should be 'Index' and trait names")
        )
      })
      
      observeEvent(length(w_user())>0, {
        req(class(w_user())=="data.frame")
        if("dt_w_clean" %in% names(val)) {
          output$warn_m <- renderText({
            "You are going to re-write the EV weight table by your uploaded file."
          })
        }
        
        val$dt_w_clean <- cleanW(w_user())
      })
      
      ebv_desc_user <- uploadTableModuleServer("upload_ebv_desc", what = rep("character", 2))
      
      output$error_m_6 <- renderText({
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
        tempVar$ebv_colClasses <- dplyr::left_join(val$dt_description_clean, tempVar$cnvrt, by = "classifier")
      })
      
      ebv_user <- uploadTableModuleServer("upload_ebv", 1, 0, 
                                          what = tempVar$ebv_colClasses$colClasses)
      
      output$error_m_7 <- renderText({
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
      
      output$error_m <- renderText({
        validate(need(!is.null(val$dt_desc_ev_clean), "Please upload a EV description file"),
                 need(!is.null(val$dt_ev_filtered), "Please upload a EV file"),
                 need(!is.null(val$dt_description_clean), "Please upload an EBV description file"),
                 need(!is.null(val$dt_ebv_filtered), "Please upload an EBV file"),
                 need(!is.null(val$dt_index), "Please finish filtering or upload an index table"),
                 # need(!is.null(val$cl$cluster_obj), 
                      # "Please finish 'run cluster' or upload a cluster object"),
                 need(!is.null(val$cl$clusters), 
                      "Please finish 'run cluster' or upload a cluster table")
        )
      })
      
      # DETECT AND SHOW WEIGHTING OPTIONS
      observeEvent(length(val$dt_w_clean) > 0, { # not sure if only works once?
# cat(" observe val$dt_w_clean\n")
        updateSelectInput(session, "choose_w", 
                          choices = c("equal weight",# "by index correlation", 
                                      names(val$dt_w_clean)[-1]),
                          selected = names(val$dt_w_clean)[2])
      })
      
      # MAKE WEIGHTS
#       # make sub-group weight:
#       # use the covariance matrix of indexes, calculate each sub-group covariances, then sum them
#       # as the denominator, then get the percentage of each sub-group--> sub-group weight
#       cluster_w <- reactive({
# # cat(" reactive cluster_w\n")
# cat(" val$dt_index: dim");print(dim(val$dt_index));print(val$dt_index[1:3,1:3])
#         req(length(input$error_m)==0,
#             !is.null(val$dt_index), !is.null(val$cl$cluster_obj), !is.null(val$cl$clusters))
#         
#         # shinyjs::show("wait")
#         
#         if(transpose) {x <- t(val$dt_index) } else { x <- as.matrix(val$dt_index)} # index x animal
#         index_cov <- cov(x, use = "pairwise.complete.obs", method = "pearson")
# # cat("  index_cov:");print(dim(index_cov));print(index_cov[1:3,1:3])        
#         cluster_w <- lapply(unique(val$cl$clusters), function(i) {
#           idx <- match(names(val$cl$clusters)[grep(i, val$cl$clusters)], colnames(index_cov))
# # cat("  i:", i, ", grep:");print(head(grep(i, val$cl$clusters)))
# # cat("   names:");print(head(names(val$cl$clusters)))
# # cat("   colnames(index_cov):");print(head(colnames(index_cov))); cat("   idx:");print(head(idx))
#           return(sum(diag(index_cov[idx, idx])))
#         })
#         
#         cluster_w <- unlist(cluster_w)/sum(unlist(cluster_w)) * 100
#         names(cluster_w) <- unique(val$cl$clusters)
#         val$cluster_w <- cluster_w
#         
#         return(data.frame(cluster = unique(val$cl$clusters), weight = cluster_w))
#       })
#       
#       renderDtTableModuleServer("cluster_w", cluster_w, extensions = "FixedHeader",
#                                 downloadName = "cluster_weight")
      
      # make individual index weight:
      # get the variance of each index in its sub-group, divide by sum of their variances?
      index_w <- reactive({
# cat(" reactive index_w\n  input$choose_w == ", input$choose_w, "\n")
# cat("  name val:");print(names(val))
        req(length(input$error_m)==0, 
            !is.null(val$dt_index), !is.null(val$cl$clusters),
            input$choose_w!="")
        
        if(transpose) {x <- t(val$dt_index) } else { x <- as.matrix(val$dt_index)} # index x animal
        index_cov <- cov(x, use = "pairwise.complete.obs", method = "pearson")
        
        index_w <- do.call(rbind, lapply(unique(val$cl$clusters), function(i) {
          idx <- match(names(val$cl$clusters)[grep(i, val$cl$clusters)], colnames(index_cov))
          
          if(length(input$choose_w)==1 && input$choose_w==""){
# cat("  if input$choose_w==''\n")
            
          } else if(length(input$choose_w)==1 && input$choose_w == "equal weight") {
            out <- rep(1/length(idx), times = length(idx))
            out <- data.frame(Index = colnames(index_cov)[idx], cluster = as.integer(i), 
                              weight = out)
            
          # } else if (length(input$choose_w)==1 && input$choose_w == "by index correlation" ) {
          #   out <- diag(index_cov[idx,idx])/sum(diag(index_cov[idx,idx]))
          #   out <- data.frame(Index = colnames(index_cov)[idx], cluster = i, weight = out)
            
          } else { # user chosen variable
            idx_row <- match(colnames(index_cov)[idx], val$dt_w_clean$Index)
            w_col <- match(input$choose_w, names(val$dt_w_clean))
            w_percent <- val$dt_w_clean[idx_row,w_col,drop = F]/colSums(val$dt_w_clean[idx_row,w_col,drop = F])
# cat("  choose_w:", input$choose_w, "val$dt_w_clean:");print(names(val$dt_w_clean))
# cat("  w_col:", class(w_col), w_col, " idx_row len:", length(idx_row), " idx len:", length(idx),
    # "\n")
            out <- rowSums(w_percent/length(w_col), na.rm = T)
# cat("  out len:", length(out), "\n")            
            out <- data.frame(Index = colnames(index_cov)[idx], cluster = as.integer(i),
                              weight = out)
          }
          
         # out$weight <- out$weight*100
          return(out)
        }))
        
        val$dt_weight <- arrange(index_w, cluster)
        return(index_w)
      })
      
      renderRctTableModuleServer("index_w", index_w, # extensions = "FixedHeader",
                                downloadName = "index_weight", digits = reactive(input$view_dec))
      
      # make aggregated EV
      # Index, cluster, trait1, trait2, ...
      ew_new <- reactive({ # 15june2021 reacted twice...
# cat("\n reactive ew_new\n")
        req(length(input$error_m)==0, !is.null(index_w()), 
            !is.null(val$dt_desc_ev_clean), !is.null(val$dt_ev_filtered))
# cat("  dt_ev_filtered:");print(dim(val$dt_ev_filtered))
# cat("  dt_desc_ev_clean:");print(dim(val$dt_desc_ev_clean))     
        idx_trait <- which(names(val$dt_ev_filtered) %in% 
                             val$dt_desc_ev_clean$column_labelling[
                               val$dt_desc_ev_clean$classifier=="EV"])
        
        ews_new <- do.call(rbind, lapply(unique(val$cl$clusters), function( i ) {
          index_names <- names(val$cl$clusters)[val$cl$clusters == i]
          index_wei <- index_w()$weight[match(index_names, index_w()$Index)]
          row_idx <- match(index_names, val$dt_ev_filtered$Index)
          ew_table <- sweep(val$dt_ev_filtered[row_idx, idx_trait], 1, index_wei, "*") # row1*index_w[1], row2*index_w[2]...
# cat("  ew_table: dim");print(dim(ew_table));print(ew_table[1:3,1:3])          
          index_new <- colSums(ew_table, na.rm = F) #/100 # vector
# cat("  index_new: i =", i, ", class", class(index_new), ", length");print(length(index_new));print(head(index_new))
          return(data.frame(Index = paste0("new_index_", i), cluster = as.integer(i), t(index_new)))
        })) %>% 
          arrange(cluster, Index)
        val$dt_ev_agg <- ews_new
        
        # make average EV
        ev_mean <- apply(val$dt_ev_filtered[,idx_trait], 2, mean, na.rm = T)
        ews_avg <- data.frame(Index = "avg_index", cluster = NA, t(ev_mean))
        val$dt_ev_avg <- ews_avg
        
        val_report[[paste0(report_prefix, "ew_new")]] <- rbind(ews_new, ews_avg)
        return(rbind(ews_new, ews_avg))
      })
      
      observeEvent({ew_new()
        input$view_dec},
      renderTableModuleServer("ew_new", ew_new, extensions = "FixedHeader",
                                downloadName = "EW_cluster_", colfilter = "none", 
                                option_list = list(sDom  = '<"top">lrt<"bottom">ip'), # disable search bar 
                                digits = reactive(input$view_dec))
      )
      
      downloadModuleServer("ew_new_t", "EW_cluster_transpose", 
                           t.data.frame(dplyr::select(ew_new(), -cluster)), row.names = T, col.names = F)
      
      # plot aggreagted EV
      output$plot_bar_newev <- renderPlot({
        withProgress(message = 'Plotting ...',
                     detail = 'This may take a while...', value = 0, {
          req(ew_new, input$font_size)
        
        width <- session$clientData[[paste0("output_", session$ns("plot_bar_newev"), "_width")]]
        
        df <- tidyr::pivot_longer(ew_new(), !c(Index, cluster), "trait", values_to = "economic_value")
        p <- plotGroupedBar(input, output, session, 
                            df, "trait", "economic_value", "Index", "Economic value($)",
                            reactive(input$font_size), baseline = T)
        
        val_report[[paste0(report_prefix, "p_ew")]] <- p
        downloadPlotModuleServer("dnld_plot_newev", "barchart_aggregated_ev", p, reactive(width))
        return(p)
      }) })
      
      # plot REW, a.k.a. aggregated EV*SD(EBV)
      output$plot_bar_newevsd <- renderPlot({
       withProgress(message = 'Plotting ...',
                    detail = 'This may take a while...', value = 0, {
          req(ew_new,  # Index, cluster, trait1, trait2, ...
              val$dt_ebv_filtered, input$font_size)
# cat(" plot_bar_newevsd\n  dt_ebv\n:");#print(head(val$dt_ebv_filtered, 1))
          width <- session$clientData[[paste0("output_", session$ns("plot_bar_newevsd"), "_width")]]

          idx <- na.omit(match(names(ew_new()), names(val$dt_ebv_filtered)))
          df_ebv <- val$dt_ebv_filtered[, idx, drop = F] # animal x trait
          ew_sub <- ew_new()[,which(names(ew_new()) %in% names(df_ebv))]
          ew_sub <- cbind(ew_new()[,1:2], ew_sub)

          rew <- sweep(ew_sub[,-c(1:2)], 2, apply(df_ebv, 2, sd, na.rm = T), "*") # col1*sd1, col2*sd2, ...
          rew <- cbind(ew_sub[, 1:2], rew)

          df <- tidyr::pivot_longer(rew, !c(Index, cluster), "trait", 
                                    values_to = "relative_economic_value")
          p <- plotGroupedBar(input, output, session,
                              df, "trait", "relative_economic_value", "Index", 
                              "Relative economic value($)", reactive(input$font_size), baseline = T)
          
          val_report[[paste0(report_prefix, "p_relew")]] <- p
          val_report[[paste0(report_prefix, "relew")]] <- rew
          
          downloadPlotModuleServer("dnld_plot_newevsd", "barchart_aggregated_relative_ev", p, 
                                   reactive(width))
          downloadModuleServer("dnld_newevsd", "aggregated_relative_ev", rew)
          return(p)
         }) })
      
    })}