aggModSidebarUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  tagList(
    h4("Upload files (optional)"),
    wellPanel(
      uploadTableModuleServer(ns("upload_ev_desc"), "EV description file"),
      span(textOutput(ns("error_m_0")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_index"), "index table (only has ID and index cols)"),
      span(textOutput(ns("error_m_1")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_cl_obj"), "cluster object .RData file"),
      span(textOutput(ns("error_m_2")), style = "color:salmon"),
      uploadTableModuleUI(ns("upload_clusters"), "cluster table"),
      span(textOutput(ns("error_m_3")), style = "color:salmon")
    )
  )
}

addModUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Aggregate indexes"),
    h2("Create index weights"),
    textOutput(ns("warn_m")),
    textOutput(ns("error_m")),
    renderDtTableModuleUI(ns("index weights")),
    tag$table(
      tags$td(downloadModuleUI(ns("dnld_weight"), "Download new index weight")),
      tags$td(downloadModuleUI(ns("dnld_ev_desc"), "Download new EV description"))
      ),
    shinyjs::hidden(span(id = ns("wait"), p("Plotting...please wait..."), style = "color:orange"))
  )
}

#' Download button server
#'
#' @param id shiny object id
#' @param val a reactive value object, containing at least 4 objects: 1) \code{dat}, a data.frame of 
#'        animal by index, e.g. 
#'        reactive(val$dt_index). If the data is index by animal, then \code{transpose} should be 
#'        set to \code{T}. 2) \code{cl_obj}, a class "hclust" or "agnes" object.
#'        3) \code{clusters}, a cluster assignment vector. e.g. a \code{cutree} output.
#'        4) \code{dt_desc_ev_clean}, a data.frame of 2 columns: column_labelling and classifier
#' @param cl a reactive function of a list containing a cluster_obj and a vector of cluster
#'        assignments
#' @param index a reactive function of a data.frame, animal ID by indexes, e.g. val$dt_index        
#' 
#' @return a .csv file
aggMod <- function(id, val, transepose = reactive(T), ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # INITIALIZE AND UPLOAD (OPTIONAL)
      tempVar <- reactiveValues()
      
      # observeEvent(input$show_corr, { shinyjs::toggle(id = "bi_clust", condition = !input$show_corr) })
      
      # when user starts the app from this step,
      # upload intermediate files to replace reactive(val$dt_index), cl()$cluster_obj, cl()$clusters
      ev_desc <- uploadTableModuleServer("upload_ev_desc")
      
      output$error_m_0 <- renderText({
        validate(
          need(class(ev_desc())!="try-error", attr(ev_desc(), "condition")$message),
          need(names(ev_desc())[1]=="ID", "Index file first column should be ID (for plant)")
        )
      })
      
      observeEvent(length(ev_desc()) > 0, { # if use ev_desc, only observe once...
        val$dt_desc_ev_clean <- ev_desc()
      })
      
      index_user <- uploadTableModuleServer("upload_index")
      
      output$error_m_1 <- renderText({
        validate(
          need(class(index_user())!="try-error", attr(index_user(), "condition")$message),
          need(names(index_user())[1]=="ID", "Index file first column should be ID (for plant)")
        )
      })
      
      observeEvent(length(index_user()) > 0, { # if use index_user, only observe once...
        # cat(" observe index_user\n  val names:");print(names(val))
        if("dt_index" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the index table by your uploaded file."
          })
        }
        out <- index_user()[,-1]
        rownames(out) <- index_user()$ID
        val$dt_index <- out
      })
      
      cl_obj_user <- uploadTableModuleServer("upload_cl_obj") # RData
      
      output$error_m_2 <- renderText({
        validate(
          need(class(cl_obj_user())!="try-error", attr(cl_obj_user(), "condition")$message),
          need(class(cl_obj_user())[1] %in% c("hclust", "agnes"), "Please upload a cluster object")
        )
      })
      
      observeEvent(length(cl_obj_user()) > 0, {
        # cat(" observe cl_obj_user\n  val names:");print(names(val))        
        if(!"cl" %in% names(val)) {
          val$cl <- NULL
          
        } else if("cl_obj" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the clustering object by your uploaded file."
          })
        }
        
        val$cl$cluster_obj <- cl_obj_user()
      })
      
      clusters_user <- uploadTableModuleServer("upload_clusters", 1, 0)
      
      output$error_m_3 <- renderText({
        validate(
          need(class(clusters_user())!="try-error", attr(clusters_user(), "condition")$message),
          need(names(clusters_user())[1]=="Index" && names(clusters_user())[2]=="cluster",
               "Cluster file headers should be 'Index' 'cluster'")
        )
      })
      
      observeEvent(length(clusters_user())>0, {
        # cat(" observe clusters_user\n  val names:");print(names(val))        
        if(!"cl" %in% names(val)) {
          val$cl <- NULL
          
        } else if("clusters" %in% names(val$cl)) {
          output$warn_m <- renderText({
            "You are going to re-write the cluster table by your uploaded file."
          })
        }
        
        out <- clusters_user()[,-1,drop = T]
        names(out) <- clusters_user()$Index
        val$cl$clusters <- out
      })
      
      output$error_m <- renderText({
        validate(need(!is.null(val$dt_desc_ev_clean), "Please upload a EV description file"),
                 need(!is.null(val$dt_index), "please finish filtering or upload an index table"),
                 need(!is.null(val$cl$cluster_obj), 
                      "please finish 'run cluster' or upload a cluster object"),
                 need(!is.null(val$cl$clusters), 
                      "Please finish 'run cluster' or upload a cluster table")
        )
      })
      
      # MAKE WEIGHTS
      # make sub-group weight:
      # use the covariance matrix of indexes, calculate each sub-group covariances, then sum them
      # as the denominator, then get the percentage of each sub-group--> sub-group weight
      cluster_w <- reactive({
cat("aggMod\n reactive cluster_w\n")
        req(length(input$error_m)==0, !is.null(val$dt_desc_ev_clean),
            !is.null(val$dt_index), !is.null(val$cl$cluster_obj), !is.null(val$cl$clusters))
        
        # shinyjs::show("wait")
        
        if(transpose) {x <- t(val$dt_index) } else { x <- as.matrix(val$dt_index)} # index x animal
        index_cov <- cov(x, use = "pairwise.complete.obs", method = "pearson")
        
        cluster_w <- lapply(unique(val$cl$cluster), function(i) {
          idx <- match(names(val$cl$clusters)[grep(i, val$cl$clusters)], colnames(index_cov))
          return(sum(diag(index_cov[idx, idx])))
        })
        names(cluster_w) <- unique(val$cl$cluster)
        cluster_w <- unlist(cluster_w)/sum(unlist(cluster_w))
        
        return(cluster_w)
      })
      
      renderDtTableModuleServer("cluster_w", cluster_w, extensions = "FixedHeader",
                                downloadName = "cluster_weight")
      
      
      # make individual index weight:
      # get the variance of each index in its sub-group, divide by sum of their variances?
    })}