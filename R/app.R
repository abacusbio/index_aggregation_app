#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# devtools::load_all()
# install.packages("https://cran.r-project.org/src/....tar.gz", type = "source", repos = NULL)
options(repos = c("CRAN" = "https://mran.microsoft.com/snapshot/2019-04-15",
                  "added" = "https://cran.rstudio.com",
                  "added1" = "https://cran.r-project.org"))
# options("repos")
# old.packages()

options(shiny.reactlog = T) # ctrl+F3
# reactlogReset()

# library(parallel)
library(shiny)
library(DT)
# library(shinyjs) # try lazy loading
library(shinyWidgets)
#library(reactlog)

library(dplyr)
library(purrr)

#library(RColorBrewer) # try lazy loading
library(ggplot2)
library(pheatmap)
# library(gplots) # heatmap.2
library(ggrepel) # geom_text_repel
library(ggdendro)
library(ggdendro)

library(cluster)
library(PMA) # spc
library(factoextra) # silouette, wss

source("modules.R", echo = F)
source("module_preprocess.R", echo = F)
source("module_dt_viewer.R")
source("module_data_filter.R")
source("module_sum_stat.R")
source("module_clustering.R", echo = F)
source("module_cl_dx.R")
source("module_cl_summary.R")
source("module_cl_weight.R")
source("module_aggregation_dx.R")
source("function_preprocess.R")
source("function_copied_selindexrevamp.R")
source("function_clean.R")
source("function_calculate_index.R")
source("function_clustering.R")
source("function_cl_dx.R")
source("function_cl_dx.R")
source("function_aggregation_dx.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  includeCSS("css/bootstrap_minty_edited.css"), # 5june2020 can use to replace tags$head()
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
           dataViewerModuleSidebarUI("ebv_filter", defaultName = "filtered_ebv"),
           # checkboxInput("ebv_na", "Include missing EBV (individuals with missing EBV will not have
                         # index values"),
           stefanFilterModUI("stfn_ebv"),
           checkboxInput("ebv_na_0", "Treat missing EBV as 0 (Otherwise animals with missing EBV
                     will not have index values)", value = F),
           span(textOutput("stefan_filter_error_message"), style = "color:salmon")
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.ebv.sumstat' && input.plant_app == 'tab.upload'",
           sumstatModSidebarUI("sumstat_ebv")
         ),
         
         conditionalPanel(
           condition = "input.upload == 'tab.step3' && input.plant_app == 'tab.upload'",
           dataViewerModuleSidebarUI("ev_filter", defaultName = "filtered_ev"),
           stefanFilterModUI("stfn_ev"),
           checkboxInput("ev_na_0", "Treat missing EV as 0 (Otherwise this trait will drop from 
                         index)", value = F),
           span(textOutput("stefan_filter_error_message_ev"), style = "color:salmon")
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
             actionButton("demo", "Run demo data", class="btn btn-secondary"),
             preprocessUploadModUI("step1")
           ),
           
           tabPanel("Step 2: Filter EBV", value = "tab.step2",
             br(),
             dataViewerModuleTabUI("ebv_filter")
           ),
           
           tabPanel("EBV summary statistics", value = "tab.ebv.sumstat",
             br(),
             sumstatModUI("sumstat_ebv")
           ),
           
           tabPanel("Step 3: Filter EV", value = "tab.step3",
             br(),
             dataViewerModuleTabUI("ev_filter")
           ),
           
           tabPanel("EV summary statistics", value = "tab.ev.sumstat",
             br(),
             sumstatModUI("sumstat_ev")
           ),
           
           tabPanel("Step 4: Combine highly correlated indexes", value = "tab.step4",
             br(),
             h2("Combine highly correlated indexes"),
             helpText("automatically, without diagnosis.")
           )
                     
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
           # clusteringSidebarUI("find_cl")
         ),
         width = 4), # sidebarPanel
       
       mainPanel(
         tabsetPanel(id = "view_index", # id can't have .
           tabPanel("View index", value = "tab.index1",
             # renderDtTableModuleUI("index1") # too long
             span(textOutput("index_view_warn"), style = "color:orange"),
             downloadModuleUI("dnld_index", "Download the index table"),
             downloadModuleUI("dnld_index_group", 
                              "Download the index and group table for your own record")
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
          
          tabPanel("Step 2: Cluster summary", value = "tab.cl.2",
            clusterSumStatModUI("cl_sumstat")
          ),
          
          tabPanel("Step 3: Cluster diagnosis", value = "tab.cl.3",
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
        width = 4), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "run_agg", # id can't have .
          tabPanel("Step 1: Make new weights", value = "tab.agg.1",
                   calWeiModUI("cl_weight")),
                    
          tabPanel("Step 2: Aggregation diagnosis", value = "tab.agg.2",
                   aggDxModUI("agg_dx")),
          
          tabPanel("Step3: Aggregation diagnosis - more", value = "tab.agg.3",
                   aggDxModUI2("agg_dx2"))
        ) # tabsetPanel run_cluster
      ), # mainPanel
      fluid = T) # sidebarLayout fluid = F doesn't work here
  ) # tabPanel Aggregation
  
  ) # nevbarPage
) # ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  ## OPTIONS ###
  # allow file sizes up to 100MB
  options(shiny.maxRequestSize = 100 * 1024 ^ 2  , shiny.trace = F
          , shiny.error = NULL #browser
          
  )
  print(Sys.getenv()[["OS"]])
  cat("N cores:", parallel::detectCores(), "\n") # rsconnect.nonprod... 8 cores/processor
  
  ## INITIALIZE, load demo ##
  val <- reactiveValues()
  
  desc_ebv <- readxl::read_xlsx("data/description_bv.xlsx", col_names = T)
  desc_ev <- read.csv2("data/description_ev.csv", sep = ",", 
                       col.names = c("column_labelling", "classifier"))
  dat_ebv <- read.table("data/bv.csv", 
                        colClasses = c(rep("character", 2), rep("double", 14)),
                        header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                        quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                        strip.white = T)
  dat_ev <- read.table("data/ev.csv", 
                       colClasses = c("character", rep("double", 11), rep("character", 2)),
                       header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                       quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                       strip.white = T)
  dat_w <- read.table("data/index_weight.csv",
                      colClasses = c("character", "double"),
                      header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                      quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                      strip.white = T)
  
  observeEvent(input$demo, {
    val$desc_ebv <- desc_ebv # column_labelling, classifier; ID, ClassVar, EBV, (Group, Order, Unit)
    val$desc_ev <- desc_ev # column_labelling, classifer; ID, ClassVar, EV, (Group, )
    val$dat_ebv <- dat_ebv # ID, (sex, RM, ...), trait1, trait2, ... (trait1_ACC, trait2_ACC...)
    val$dat_ev <- dat_ev # ID, (line, group1), ... trait1, trait2, ...
    val$dat_w <- dat_w # Index weight (weight2) ...
# cat("observe input$demo val"); print(sapply(reactiveValuesToList(isolate(val)), head)    )
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
  sumstatMod("sumstat_ev", reactive(val$dt_ev_filtered))
  
  ## CALCULATE INDEX ##
  
  # Create val$dt_index
  observeEvent(input$plant_app, { # react when change to other tabs as well
cat("observe plant_app dt_sub_index_ids\n");print(names(val))
    req(input$plant_app == 'tab.index' && input$view_index == "tab.index1")
    req(length(reactiveValuesToList(val)) <= 12 && 
          length(reactiveValuesToList(val)) >=10) # avoid re-calculate when downstream analysis is aready triggered
    req(val$dt_ev_filtered, val$dt_ebv_filtered, val$dt_description_clean, val$dt_desc_ev_clean)

    output$index_view_warn <- renderText({"Creating index, please wait..."}) # never showed
    
    # ID, sex, ..., trait1, trait2, ... index1, index2, ...
    val$dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session,
                          val$dt_ebv_filtered, val$dt_ev_filtered, val$dt_description_clean,
                          val$dt_desc_ev_clean)
    
    # ID, sex, ... index1, index2...
    val$dt_sub_index_ids <- 
      val$dt_sub_ebv_index_ids[,!names(val$dt_sub_ebv_index_ids) 
                               %in% val$dt_description_clean$column_labelling[
                                 val$dt_description_clean$classifier=="EBV"] ]
    
    # animal ID x Index
    val$dt_index <- dplyr::select(val$dt_sub_index_ids, matches(val$dt_ev_filtered$Index)) 
    # %>% t() %>% data.frame()
    # names(val$dt_index) <- val$dt_sub_index_ids$ID # rownames auto get from original colnames
# cat("observe plant_app, val$dt_index:");print(dim(val$dt_index    ))
  })
  
  ## INDEX STATISTICS ##
  # takes long time to load
  observeEvent(val$dt_index, { #"dt_index" %in% names(val), {
# cat("observe dt_index, val: ");print(names(val))
    req(!is.null(val$dt_index))
    output$index_view_warn <- renderText({
    "Index calculated. Now you can download and go to the next steps."})
  })
  # renderDtTableModuleServer("index1", reactive(val$dt_sub_index_ids), T, downloadName = "index")
  downloadModuleServer("dnld_index", "index", 
                       data.frame(ID = rownames(val$dt_index), val$dt_index), F, "csv")
  downloadModuleServer("dnld_index_group", "index_group", val$dt_sub_index_ids, F, "csv")
  
  
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
  clusterSumStatMod("cl_sumstat", val, transpose = F)
  
  ## CLUSTER DIAGNOSIS ##
  
  # need val$dt_index, val$cl. if upload new files, fill val$cl with list(cluster_obj, clusters)
  clusterDxMod("Dx", val, 
               transpose = T, reactive(input$`find_cl-center`), reactive(input$`find_cl-scale`))
  
  ## AGGREGATION ##
  
  # CREATE INDEX WEIGHT GIVEN CLUSTERING RESULTS #
  # need val$dt_index,  val$cl, val$dt_ev_filtered, val$dt_desc_ev_clean (for user group)
  # create val$dt_weight (data.frame), a data.frame of 3 columns: Index, cluster and weight; and 
  # val$dt_ev_agg, a data.frame of columns as Index, cluster and traits
  calWeiMod("cl_weight", val, transpose = F)
  
  # AGGREGATED INDEX DIAGNOSIS #
  # look at correlations among new and old indexes, and top individual overlap among them
  # need val$dt_ev_agg, val$dt_ebv_filtered, val$dt_description_clean
  aggDxMod("agg_dx", val, transpose = F, reactive(val$cl$clusters), reactive(val$dt_ev_agg),
           reactive(val$dt_index))
  
  # look at classVar pattern among indexes
  aggDxMod2("agg_dx2", val, transpose = F, reactive(val$cl$clusters))
} # server

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
# runApp(appDir = "R")

shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020