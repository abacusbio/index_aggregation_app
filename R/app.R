
# rsconnect::writeManifest("R")

# devtools::load_all()
# install.packages("https://cran.r-project.org/src/....tar.gz", type = "source", repos = NULL)
options(repos = c("CRAN" = "https://mran.microsoft.com/snapshot/2019-04-15",
                  "added" = "https://cran.rstudio.com",
                  "added1" = "https://cran.r-project.org"))
# options("repos")
# old.packages()

# options(shiny.reactlog = T) # ctrl+F3
# reactlogReset()

# library(parallel)
library(shiny)
library(DT)
library(shinyWidgets)
# library(shinyjs) # lazy loading
# library(htmltools) # lazy loading

#library(reactlog)

library(dplyr)
library(purrr)

#library(RColorBrewer) # lazy loading
library(ggplot2)
library(pheatmap)
# library(gplots) # heatmap.2
library(ggrepel) # geom_text_repel
library(ggdendro)

library(cluster)
library(PMA) # spc
library(factoextra) # silouette, wss

source("modules.R", echo = F)
source("module_preprocess.R", echo = F)
source("module_dt_viewer.R")
source("module_data_filter.R")
source("module_sum_stat.R")
source("module_index_sumstat.R")
source("module_clustering.R", echo = F)
source("module_cl_dx.R")
source("module_cl_summary.R")
source("module_cl_weight.R")
source("module_aggregation_dx.R")
source("function_preprocess.R")
source("function_copied_selindexrevamp.R")
source("function_clean.R")
source("function_sum_stat.R")
source("function_index_sumstat.R")
source("function_calculate_index.R")
source("function_clustering.R")
source("function_cl_dx.R")
source("function_cl_weight.R")
source("function_aggregation_dx.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  htmltools::includeCSS("css/bootstrap_minty_edited.css"), # 5june2020 can use to replace tags$head()
  tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        /*font-weight: bold;*/
      }
    "))
  ),
    
    # Application title
  titlePanel("Plant Index Aggregation App"),

  navbarPage("AbacusBio", id = "plant_app",
   # navbarMenu("File upload", menuName = "menu.file.upload", # comment out if don't want dropdown menu
               # "----",
               #  "Survey Gizmo file preprocess", # section header
  
   tabPanel( # on the dropdown list of navbarmenu
     "Preprocess Files", value = 'tab.upload',
     # Sidebar on the left
     sidebarLayout(
       sidebarPanel(
         conditionalPanel(
           condition = "input.upload == 'tab.step1' && input.plant_app == 'tab.upload'",
           preprocessUploadModsidebarUI("step1")
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.step2' && input.plant_app == 'tab.upload'",
           column(12, div(actionButton("help_btn_ebv_filter", "", icon("question"), 
                                       class = "btn btn-outline-info"), style = "float:right")),
           dataViewerModuleSidebarUI("ebv_filter", defaultName = "filtered_ebv"),
           # checkboxInput("ebv_na", "Include missing EBV (individuals with missing EBV will not have
                         # index values"),
           stefanFilterModUI("stfn_ebv"),
           span(textOutput("stefan_filter_error_message"), style = "color:salmon"),
           checkboxInput("ebv_na_0", "Treat missing EBV as 0 (Otherwise individuals with missing EBV
                     will not have index values)", value = F),
           shinyjs::hidden(
             span(id = "warn_ebv_0", 
                  p("Please be sure all your EBV are distributed around 0 before ticking this box"), 
                  class = "text-danger"))
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.ebv.sumstat' && input.plant_app == 'tab.upload'",
           sumstatModSidebarUI("sumstat_ebv")
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.step3' && input.plant_app == 'tab.upload'",
           column(12, div(actionButton("help_btn_ev_filter", "", icon("question"), 
                                       class = "btn btn-outline-info"), style = "float:right")),
           dataViewerModuleSidebarUI("ev_filter", defaultName = "filtered_ev"),
           stefanFilterModUI("stfn_ev"),
           span(textOutput("stefan_filter_error_message_ev"), style = "color:salmon"),
           checkboxInput("ev_na_0", "Treat missing EV as 0 (Otherwise this trait will drop from 
                         index)", value = F)
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.ev.sumstat' && input.plant_app == 'tab.upload'",
           sumstatModSidebarUI("sumstat_ev")
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.step4' && input.plant_app == 'tab.upload'",
           h4("sidebar title")
         ),
         
         width = 4), # sidebarPanel, class = "bg-primary"),
       
       mainPanel(
         tabsetPanel(id = "upload", # id can't have .
           tabPanel("Step 1: Upload", value = "tab.step1",
             br(),
            # shinyjs::show(
               span(id = "initial_warn", 
                    p("Please wait a couple of seconds before clicking anywhere. This allows the app to load/calculate
    first, otherwise it can result in error or crash. For example, let the table/picture display 
    before clicking the 'Filter and save' button."),  class = "text-warning"), # style="background-color:bg-danger"), # class = "bg-danger"), #
             actionButton("demo", "Run demo data", class="btn btn-secondary"),
             preprocessUploadModUI("step1")
           ),
           
           tabPanel("Step 2: Filter EBV", value = "tab.step2",
             br(),
             shinyjs::hidden(div(id = "help_html_ebv_filter",
                                 # htmltools::includeMarkdown("help/preprocess_filter_ebv.Rmd"))), # doesn't knit table or plot, and show codes even when echo = F
                                 includeHTML(knitr::knit2html("help/preprocess_filter_ebv.Rmd", # this doeson't use pandoc. can be absolete soon
                                                              fragment.only = TRUE,
                                                              options = c("toc"))))),
             # another solution: https://stackoverflow.com/questions/50429119/sidebarmenu-does-not-function-properly-when-using-includehtml/50820737#50820737
             dataViewerModuleTabUI("ebv_filter")
           ),
           
           tabPanel("EBV summary statistics", value = "tab.ebv.sumstat",
             br(),
             sumstatModUI("sumstat_ebv")
           ),
           
           tabPanel("Step 3: Filter EV", value = "tab.step3",
             br(),
             shinyjs::hidden(div(id = "help_html_ev_filter",
                                 includeHTML(knitr::knit2html("help/preprocess_filter_ebv.Rmd",
                                                              fragment.only = TRUE,
                                                              options = c("toc"))))),
             dataViewerModuleTabUI("ev_filter")
           ),
           
           tabPanel("EV summary statistics", value = "tab.ev.sumstat",
             br(),
             sumstatModUI("sumstat_ev")
           ) #,
           
           # tabPanel("Step 4: Combine highly correlated indexes", value = "tab.step4",
           #   br(),
           #   h2("Combine highly correlated indexes"),
           #   helpText("automatically, without diagnosis.")
           # )
                     
         ) # tabsetPanel upload
       ),# class = "bg-light"), # mainPanel
       fluid = T) # sidebarLayout fluid = F doesn't work here
   ), # tabPanel Upload Files
   # ), # navbarMenu File upload
   
   tabPanel( # on the dropdown list of navbarmenu
     "Index Viewer", value = 'tab.index',
     # Sidebar on the left
     sidebarLayout(
       sidebarPanel(
         conditionalPanel(
           condition = "input.view_index == 'tab.index1' && input.plant_app == 'tab.index'",
           column(12, div(actionButton("help_btn_index", "", icon("question"), 
                                       class = "btn btn-outline-info"), style = "float:right")),
           indexSumstatModSidebarUI("index_view")
         ),
         width = 4), # sidebarPanel
       
       mainPanel(
         tabsetPanel(id = "view_index", # id can't have .
           tabPanel("View index", value = "tab.index1",
             # renderDtTableModuleUI("index1") # too long
             br(),
             shinyjs::hidden(div(id = "help_html_index",
                                 includeHTML(knitr::knit2html("help/index_viewer.Rmd",
                                                              fragment.only = TRUE)))),
             span(textOutput("index_view_warn"), class = "text-success"),
             verbatimTextOutput("index_view_n"),
             indexSumstatModUI("index_view")
             )
                     
         ) # tabsetPanel view_index
         
       ), # mainPanel
       fluid = T) # sidebarLayout fluid = F doesn't work here
   ), # tabPanel Index viewer
    
  #  navbarMenu("Clustering", menuName = "menu.cluster",
               # "----",
               #  "Survey Gizmo file preprocess", # section header
  tabPanel( # on the dropdown list of navbarmenu
    "Clustering Analysis", value = 'tab.cluster',
    # Sidebar on the left
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.run_cluster == 'tab.cl.1' && input.plant_app == 'tab.cluster'",
          clusteringModSidebarUI("find_cl")
        ),
        
        conditionalPanel(
          condition = "input.run_cluster == 'tab.cl.2' && input.plant_app == 'tab.cluster'",
          clusterSumStatModSidebarUI("cl_sumstat")
        ),
        
        conditionalPanel(
          condition = "input.run_cluster == 'tab.cl.3' && input.plant_app == 'tab.cluster'",
          clusterDxModSidebarUI("Dx")
        ),
        width = 4), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "run_cluster", # id can't have .
          tabPanel("Step 1: run cluster", value = "tab.cl.1",
            clusteringModUI("find_cl")     
            ),
          
          tabPanel("Cluster summary", value = "tab.cl.2",
            clusterSumStatModUI("cl_sumstat")
          ),
          
          tabPanel("Cluster diagnosis", value = "tab.cl.3",
            clusterDxModUI("Dx")
          )
                    
        ) # tabsetPanel run_cluster
        
      ), # mainPanel
      fluid = T) # sidebarLayout fluid = F doesn't work here
  ), # tabPanel Clustering analysis
  #  ) # navbarMenu Clustering
  
  #  navbarMenu("Clustering", menuName = "menu.cluster",
  # "----",
  #  "Survey Gizmo file preprocess", # section header
  tabPanel( # on the dropdown list of navbarmenu
    "Index Aggregation", value = 'tab.agg',
    # Sidebar on the left
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.run_agg == 'tab.agg.1' && input.plant_app == 'tab.agg'",
          calWeiModSidebarUI("cl_weight")
        ),
        
        conditionalPanel(
          condition = "input.run_agg == 'tab.agg.2' && input.plant_app == 'tab.agg'",
          aggDxModSidebarUI("agg_dx")
        ),
        
        conditionalPanel(
          condition = "input.run_agg == 'tab.agg.3' && input.plant_app == 'tab.agg'",
          aggDxModSidebarUI2("agg_dx2")
        ),
        
        conditionalPanel(
          condition = "input.run_agg == 'tab.agg.4' && input.plant_app == 'tab.agg'",
          aggDxModSidebarUI3("agg_dx3")
        ),
        conditionalPanel(
          condition = "input.run_agg == 'tab.agg.5' && input.plant_app == 'tab.agg'",
          aggDxModSidebarUI4("agg_dx4")
        ),
        width = 4), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "run_agg", # id can't have .
          tabPanel("Step 1: Make new indexes", value = "tab.agg.1",
                   calWeiModUI("cl_weight")),
                    
          tabPanel("Aggregation diagnosis-correlation", value = "tab.agg.2",
                   aggDxModUI("agg_dx")),
          
          tabPanel("Aggregation diagnosis-top individual", value = "tab.agg.3",
                   aggDxModUI2("agg_dx2")),
          
          tabPanel("Aggregation diagnosis-variable pattern", value = "tab.agg.4",
                   aggDxModUI3("agg_dx3")),
          
          tabPanel("Aggregation diagnosis-weighting pattern", value = "tab.agg.5",
                   aggDxModUI4("agg_dx4"))
        ) # tabsetPanel run_cluster
      ), # mainPanel
      fluid = T) # sidebarLayout fluid = F doesn't work here
  ) # tabPanel Aggregation
  
  ) # nevbarPage
) # ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  ## OPTIONS ###
  # allow file sizes up to 300MB
  options(shiny.maxRequestSize = 300 * 1024 ^ 2, shiny.trace = F
          , shiny.error = NULL, # browser 
          rsconnect.max.bundle.size = 8192 * 1024^2 # largest memory size available
          
  )
  # print(Sys.getenv())
  cat("N cores:", parallel::detectCores(), "\n") # rsconnect.nonprod... 8 cores/processor
  
  ## INITIALIZE, load demo ##
  shinyjs::hide("initial_warn", T, "fade", 10)
  
  val <- reactiveValues()
  
  observeEvent(input$demo, {
    # column_labelling, classifier; ID, ClassVar, EBV, (Group, Order, Unit)
    val$desc_ebv <- readxl::read_xlsx("data/description_bv.xlsx", col_names = T) 
    # column_labelling, classifer; ID, ClassVar, EV, (Group, )
    val$desc_ev <- read.csv2("data/description_ev.csv", sep = ",", 
                             col.names = c("column_labelling", "classifier"))
    # ID, (sex, RM, ...), trait1, trait2, ... (trait1_ACC, trait2_ACC...)
    val$dat_ebv <- read.table("data/bv.csv", 
                              colClasses = c(rep("character", 2), rep("double", 14)),
                              header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                              quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                              strip.white = T)
    # ID, (line, group1), ... trait1, trait2, ...
    val$dat_ev <- read.table("data/ev.csv", 
                             colClasses = c("character", rep("double", 11), rep("character", 2)),
                             header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                             quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                             strip.white = T)
    # Index weight (weight2) ...
    val$dat_w <- read.table("data/index_weight.csv",
                            colClasses = c("character", "double"),
                            header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                            quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                            strip.white = T)
# cat("observe input$demo val"); print(sapply(reactiveValuesToList(isolate(val)), head)    )
  })
  
  ### SHOW/HIDE HELP .Rmd ###
  observeEvent(input$help_btn_ebv_filter, {
    if(input$help_btn_ebv_filter %% 2 == 1){
      shinyjs::show("help_html_ebv_filter")
    }else{
      shinyjs::hide("help_html_ebv_filter")
    }
  })
  
  observeEvent(input$help_btn_ev_filter, {
    if(input$help_btn_ev_filter %% 2 == 1){
      shinyjs::show("help_html_ev_filter")
    }else{
      shinyjs::hide("help_html_ev_filter")
    }
  })
  
  observeEvent(input$help_btn_index, {
    if(input$help_btn_index %% 2 == 1){
      shinyjs::show("help_html_index")
    }else{
      shinyjs::hide("help_html_index")
    }
  })
  
  ## UPLOAD DATA ##
  # return val$desc_ebv, val$desc_ev, val$dat_ebv, val$dat_ev and/or val$dat_w
  preprocessUploadMod("step1", val) # reactive(val))
  
  ## CLEAN DATA ##
  # change format etc
  observeEvent(input$upload == 'tab.step2' || input$upload == 'tab.step3', {
# cat("observe tab.step2\n"); print(names(val));print(length(val));# val is not a list
# print(length(reactiveValuesToList(val))); print(sapply(reactiveValuesToList(isolate(val)), length))
    req(length(reactiveValuesToList(val)) <= 5 && length(reactiveValuesToList(val)) >= 4) # if uploaded new files after calculations, won't react
    req(length(val$desc_ebv) > 0 && length(val$desc_ev) > 0 && 
          length(val$dat_ebv) > 0 && length(val$dat_ev) > 0)
# cat(" req2 satisfied\n")
    val$dt_description_clean <- cleanDescData(val$desc_ebv)
    val$dt_ebv_clean <- cleanEbvData(val$dt_description_clean, val$dat_ebv)
    val$dt_desc_ev_clean <- val$desc_ev
    val$dt_ev_clean <- cleanEVplant(val$dt_desc_ev_clean, val$dat_ev)
    
    if("dat_w" %in% names(val)) {
      val$dt_w_clean <- cleanW(val$dat_w) 
    }
  })
    
  ## FILTER EBV ##
  # NA filter is on the UI (ebv_na, acc_na)

  # character/factor filter
  stefanFilterMod("stfn_ebv", dt = reactive(val$dt_ebv_clean))
  
  filter_levels <- eventReactive(input$`stfn_ebv-apply`, {
    # print("event reactive stefan button")
    req(val$dt_ebv_clean)
# cat("event reactive stefan button\n")
    req(!is.null(input$`stfn_ebv-filter_col`))
    
    filter_cols <- input$`stfn_ebv-filter_col`
    
    filter_levels <- lapply(seq(filter_cols), function(i) {
      return(input[[paste0("stfn_ebv-", i)]])
    })
    names(filter_levels) <- seq(filter_cols)
    
    if(any(sapply(filter_levels, length)==0)) { # sanity check
      output$stefan_filter_error_message <- renderUI({
        renderText("Please select filter levels to apply")
      })
    }
    return(filter_levels)
  })
  
  # show and download data table. Apply column filters and the search bar.
  # the updated datatable is stored in val$data_filtered
  dataViewerModuleServer("ebv_filter", reactive(val$dt_ebv_clean), val,
                         filter_dat_name = "dt_ebv_filtered",
                         filter_cols = reactive(input$`stfn_ebv-filter_col`),
                         filter_levels = filter_levels,
                         na_include = reactive(input$ebv_na_0), na_to_0 = reactive(input$ebv_na_0),
                         apply = reactive(input$`stfn_ebv-apply`)
  )
  
  observeEvent(input$ebv_na_0, {
    if(input$ebv_na_0) {
      shinyjs::show("warn_ebv_0")
    } else {
      shinyjs::hide("warn_ebv_0")
    }
  })
  
  ## SUMMARY STATISTICS EBV ##
  sumstatMod("sumstat_ebv", reactive(val$dt_ebv_filtered))
  
  ## FILTER EV ##
  # NA filter is on the UI (ebv_na, acc_na)
  
  # character/factor filter
  stefanFilterMod("stfn_ev", dt = reactive(val$dt_ev_clean))
  
  filter_levels_ev <- eventReactive(input$`stfn_ev-apply`, {
    # print("event reactive stefan button")
    req(val$dt_ev_clean)
    # cat("event reactive stefan button\n")
    req(!is.null(input$`stfn_ev-filter_col`))
    
    filter_cols <- input$`stfn_ev-filter_col`
    
    filter_levels <- lapply(seq(filter_cols), function(i) {
      return(input[[paste0("stfn_ev-", i)]])
    })
    names(filter_levels) <- seq(filter_cols)
    
    if(any(sapply(filter_levels, length)==0)) { # sanity check
      output$stefan_filter_error_message_ev <- renderUI({
        renderText("Please select filter levels to apply")
      })
    }
    return(filter_levels)
  })
  
  # show and download data table. Apply column filters and the search bar.
  # the updated datatable is stored in val$dt_ev_filtered & val$dt_ebv_filtered
  dataViewerModuleServer("ev_filter", reactive(val$dt_ev_clean), val,
                         filter_dat_name = "dt_ev_filtered",
                         filter_cols = reactive(input$`stfn_ev-filter_col`),
                         filter_levels = filter_levels_ev,
                         na_include = reactive(input$ev_na_0), na_to_0 = reactive(input$ev_na_0),
                         apply = reactive(input$`stfn_ev-apply`)
  )
  
  ## SUMMARY STATISTICS EBV ##
  sumstatMod("sumstat_ev", reactive(val$dt_ev_filtered), xlab = "Economic Value ($)")
  
  ## CALCULATE INDEX ##
  
  # Create val$dt_index
 observeEvent(input$plant_app, { # react when change to other tabs as well
  # observeEvent( length(val$dt_ev_filtered)>0 && length(val$dt_ebv_filtered) > 0, {
# cat("observe plant_app\n len val:", length(reactiveValuesToList(val)),"\n");#print(names(val))
# cat(" plant_app:", input$plant_app, "\n")
    req(input$plant_app == 'tab.index' && input$view_index == "tab.index1")
    req(length(reactiveValuesToList(val)) <= 12 && 
          length(reactiveValuesToList(val)) >=10) # avoid re-calculate when downstream analysis is aready triggered
    req(val$dt_ev_filtered, val$dt_ebv_filtered, val$dt_description_clean, val$dt_desc_ev_clean)
# cat(" reqs satisfied\n")
# cat(" dt_ev_filtered:");print(val$dt_ev_filtered$Index)
    output$index_view_warn <- renderText({"Creating index, please wait..."})
    
    # ID, sex, ..., trait1, trait2, ... index1, index2, ...
    val$dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session,
                          val$dt_ebv_filtered, val$dt_ev_filtered, val$dt_description_clean,
                          val$dt_desc_ev_clean)

    # ID, sex, ... index1, index2...
    val$dt_sub_index_ids <- 
      val$dt_sub_ebv_index_ids[,!names(val$dt_sub_ebv_index_ids)
                               %in% val$dt_description_clean$column_labelling[
                                 val$dt_description_clean$classifier=="EBV"] ]
# cat(" dt_sub_index_ids:");print(tail(colnames(val$dt_sub_index_ids), -2))    
    # animal ID x Index
    val$dt_index <- dplyr::select(val$dt_sub_index_ids, matches(val$dt_ev_filtered$Index))
    # %>% t() %>% data.frame()
    rownames(val$dt_index) <- val$dt_sub_index_ids$ID # rownames may not auto get from original colnames
# print(match(names(val$dt_sub_index_ids), val$dt_ev_filtered$Index))
# cat("observe plant_app, dim val$dt_index:");print(dim(val$dt_index));print(val$dt_index[1:3,1:3])
    output$index_view_warn <- renderText({
      "Index calculated. Now you can download and go to the next steps."})
  })
  
  ## INDEX STATISTICS ##
  observeEvent(val$dt_index, { #"dt_index" %in% names(val), { # only observe once
# cat("observe dt_index, val: ");print(names(val))
    req(!is.null(val$dt_index))
    
    output$index_view_n <- renderPrint({
    paste0("You have ", nrow(val$dt_index), " individuals and ", ncol(val$dt_index), " indexes. ",
           "Individuals with missing EBVs are removed.")
      })
  })
  indexSumstatMod("index_view", reactive(val$dt_index), val)
  
  ## Find CLUSTER ##
  
  # # a smaller data
  # votes.repub <- cluster::votes.repub[
  #   which(apply(cluster::votes.repub, 1, is.na) %>% apply(2, sum) ==0),] # states by features
  # 
  # observeEvent(votes.repub,{
  #   req(length(votes.repub) > 0)
  #   val$dt_index <- data.frame(t(votes.repub)) # feature x states
  #   print(dim(val$dt_index))
  #   })
  # 
  # cl <- clusteringMod("find_cl", val, dat = reactive(val$dt_index), transpose = F)

  # return list(cluster_obj, clusters). Create val$cl.
  # if didn't run finalCluster will return list(cluster_obj, best_method, agg_coefs)
  # with the simulation it takes 6 min to find an agglomerative method, 6.5 min to run wss for k,
  # and 6 min to run silhouette for k
  cl <- clusteringMod("find_cl", val,
                dat = reactive(val$dt_index), # col_sel = reactive(val$dt_ev_filtered$Index),
                transpose = F)
  
  ## CLUSTER SUMMARY STATISTICS ##
  
  # need val$dt_index, val$cl
  clusterSumStatMod("cl_sumstat", val, cl, transpose = F)
  
  ## CLUSTER DIAGNOSIS ##
  
  # need val$dt_index, val$cl. if upload new files, fill val$cl with list(cluster_obj, clusters)
  clusterDxMod("Dx", val, 
               transpose = T, reactive(input$`find_cl-center`), reactive(input$`find_cl-scale`))
  
  ## AGGREGATION ##
  
  # CREATE INDEX WEIGHT GIVEN CLUSTERING RESULTS #
  # need val$dt_index,  val$cl, val$dt_ev_filtered, val$dt_desc_ev_clean (for user group)
  # create val$dt_weight (data.frame), a data.frame of 3 columns: Index, cluster and weight;
  # val$dt_ev_agg, a data.frame of columns as Index, cluster and traits (EVs); and
  # val$dt_ev_avg, same structure as above
  calWeiMod("cl_weight", val, transpose = F) # 15june2021 this occurred twice. 2nd time new_index_1 are NAs
  
  # AGGREGATED INDEX DIAGNOSIS #
  
  # look at correlations among new and old indexes, and top individual overlap among them
  # need val$dt_ev_agg, val$dt_ebv_filtered, val$dt_description_clean
  # create val$dt_index_new. The same as dt_index but with new_index in it.
  dt_index_sub <- aggDxMod("agg_dx", val, transpose = F, reactive(val$cl$clusters), 
                           reactive(val$dt_ev_agg), reactive(val$dt_index))
  
  # top individual overlap/agreement
  aggDxMod2("agg_dx2", transpose = F, 
           dt_index_sub, reactive(val$dt_index),
           reactive(input$`agg_dx-sel_index`), reactive(input$`agg_dx-sel_agg`),
           reactive(input$`agg_dx-sel_cluster`) 
          )
  
  # look at classVar pattern among aggregated indexes and user-defined groups
  # need val$dt_index_new and others in val.
  aggDxMod3("agg_dx3", val, transpose = F, reactive(val$cl$clusters))
  
  # look at EV weighting pattern among aggregated indexes and user-defined groups
  # need val$dt_w_clean
  # need val$dt_ev_agg, a data.frame of columns as Index, cluster and traits (EVs); and
  # val$dt_ev_avg, same structure as above
  aggDxMod4("agg_dx4", val, 
            reactive(val$dt_ev_agg), reactive(val$dt_w_clean), reactive(val$cl$clusters))
} # server

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
# runApp(appDir = "R")

shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020