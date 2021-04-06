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

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
library(shiny)
library(DT)
library(shinyjs)
library(shinyWidgets)

library(dplyr)
library(purrr)

library(ggplot2)
library(gplots) # heatmap.2
library(ggrepel) # geom_text_repel
library(ggdendro)

library(cluster)
library(PMA) # spc
library(factoextra) # silouette, wss

source("modules.R", echo = F)
source("module_preprocess.R", echo = F)
source("module_clustering.R", echo = F)
source("module_data_filter.R")
source("module_dt_viewer.R")
source("function_preprocess.R")
source("function_copied_selindexrevamp.R")
source("function_clean.R")
source("function_calculate_index.R")
source("function_clustering.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: salmon;
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
           condition = "input.upload == 'tab.step3' && input.plant_app == 'tab.upload'",
           dataViewerModuleSidebarUI("ev_filter", defaultName = "filtered_ev"),
           stefanFilterModUI("stfn_ev"),
           checkboxInput("ev_na_0", "Treat missing EV as 0 (Otherwise this trait will drop from 
                         index)", value = F),
           span(textOutput("stefan_filter_error_message_ev"), style = "color:salmon")
         ),
         
         width = 3), # sidebarPanel
       
       mainPanel(
         tabsetPanel(id = "upload", # id can't have .
           tabPanel("Step 1: Upload", value = "tab.step1",
             br(),
             actionButton("demo", "Run demo data"),
             preprocessUploadModUI("step1")
           ),
           
           tabPanel("Step 2: Filter EBV", value = "tab.step2",
             br(),
             dataViewerModuleTabUI("ebv_filter")
           ),
           
           tabPanel("Step 3: Filter EV", value = "tab.step3",
             br(),
             dataViewerModuleTabUI("ev_filter")
           )
                     
         ) # tabsetPanel upload
       ), # mainPanel
       fluid = T) # sidebarLayout fluid = F doesn't work here
   ), # tabPanel Upload Files
   # ), # navbarMenu File upload
   
   tabPanel( # on the dropdown list of navbarmenu
     "Index viewer", value = 'tab.index',
     # Sidebar on the left
     sidebarLayout(
       sidebarPanel(
         conditionalPanel(
           condition = "input.view_index == 'tab.index1' && input.plant_app == 'tab.index'",
           # clusteringSidebarUI("find_cl")
         ),
         width = 3), # sidebarPanel
       
       mainPanel(
         tabsetPanel(id = "view_index", # id can't have .
           tabPanel("View index", value = "tab.index1",
             renderDtTableModuleUI("index1")
             )
                     
         ) # tabsetPanel view_index
         
       ), # mainPanel
       fluid = T) # sidebarLayout fluid = F doesn't work here
   ), # tabPanel Index viewer
    
  #  navbarMenu("Clustering", menuName = "menu.cluster",
               # "----",
               #  "Survey Gizmo file preprocess", # section header
  tabPanel( # on the dropdown list of navbarmenu
    "Clustering analysis", value = 'tab.cluster',
    # Sidebar on the left
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.run_cluster == 'tab.stepn' && input.plant_app == 'tab.cluster'",
          clusteringSidebarUI("find_cl")
        ),
        width = 3), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "run_cluster", # id can't have .
                    tabPanel("Step n: run cluster", value = "tab.stepn",
                             clusteringUI("find_cl")     
                    )
                    
        ) # tabsetPanel run_cluster
        
      ), # mainPanel
      fluid = T) # sidebarLayout fluid = F doesn't work here
  ) # tabPanel Clustering analysis
  #  ) # navbarMenu Clustering
  ) # nevbarPage
) # ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  ## OPTIONS ###
  # allow file sizes up to 100MB
  options(shiny.maxRequestSize = 100 * 1024 ^ 2  , shiny.trace = F # F
          , shiny.error = NULL #browser
  )
  
  ## INITIALIZE, load demo ##
  val <- reactiveValues()
  
  desc_ebv <- readxl::read_xlsx("../data/description_bv.xlsx", col_names = T)
  desc_ev <- read.csv2("../data/description_ev.csv", sep = ",", 
                       col.names = c("column_labelling", "classifier"))
  dat_ebv <- read.table("../data/sire_bv.csv", 
                        colClasses = c(rep("character", 2), rep("double", 14)),
                        header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                        quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                        strip.white = T)
  dat_ev <- read.table("../data/sire_ev.csv", 
                       colClasses = c("character", rep("double", 11), "character"),
                       header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                       quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                       strip.white = T)
  
  observeEvent(input$demo, {
    val$desc_ebv <- desc_ebv # column_labelling, classifier; ID, ClassVar, EBV, (Group, Order, Unit)
    val$desc_ev <- desc_ev # column_labelling, classifer; ID, ClassVar, EV, (Group, )
    val$dat_ebv <- dat_ebv # ID, (sex, RM, ...), trait1, trait2, ... (trait1_ACC, trait2_ACC...)
    val$dat_ev <- dat_ev # ID, (line, group1), ... trait1, trait2, ...
# cat("observe input$demo val"); print(sapply(reactiveValuesToList(isolate(val)), head)    )
  })
  
  ## UPLOAD DATA ##
  # flag_upload <- 
  preprocessUploadMod("step1", val) # reactive(val))
  
  ## CLEAN DATA ##
  # change format etc
  observeEvent(input$upload == 'tab.step2' || input$upload == 'tab.step3', {
# cat("observe tab.step2\n"); print(names(val));print(length(val));# val is not a list
# print(length(reactiveValuesToList(val))); print(sapply(reactiveValuesToList(isolate(val)), length))
    req(length(reactiveValuesToList(val)) <= 5 && length(reactiveValuesToList(val)) >=4) # if uploaded new files after calculations, won't react
    req(length(val$desc_ebv) > 0 && length(val$desc_ev) > 0 && 
          length(val$dat_ebv) > 0 && length(val$dat_ev) > 0)
# cat(" req2 satisfied\n")
    val$dt_description_clean <- cleanDescData(val$desc_ebv)
    val$dt_ebv_clean <- cleanEbvData(val$dt_description_clean, val$dat_ebv)
    val$dt_desc_ev_clean <- val$desc_ev
    val$dt_ev_clean <- cleanEVplant(val$dt_description_clean, val$dat_ev)
    
    if("dat_w" %in% names(val)) {
      val$dt_w_clean <- cleanW(val$dt_w_clean) 
    }
  })
    
  ## FILTER EBV ##
  # NA filter is on the UI (ebv_na, acc_na)

  # character/factor filter
  stefanFilterMod("stfn_ebv", dt = reactive(val$dt_ebv_clean))
  
  filter_levels <- eventReactive(input$`stfn_ebv-stefan_button`, {
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
                         na_include = reactive(input$ebv_na_0), na_to_0 = reactive(input$ebv_na_0)
  )
  
  ## FILTER EV ##
  # NA filter is on the UI (ebv_na, acc_na)
  
  # character/factor filter
  stefanFilterMod("stfn_ev", dt = reactive(val$dt_ev_clean))
  
  filter_levels_ev <- eventReactive(input$`stfn_ebv-stefan_button`, {
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
  # the updated datatable is stored in val$data_filtered
  dataViewerModuleServer("ev_filter", reactive(val$dt_ev_clean), val,
                         filter_dat_name = "dt_ev_filtered",
                         filter_cols = reactive(input$`stfn_ev-filter_col`),
                         filter_levels = filter_levels_ev,
                         na_include = reactive(input$ev_na_0), na_to_0 = reactive(input$ev_na_0)
  )
  
  ## CALCULATE INDEX ##
  observeEvent(input$plant_app == 'tab.index', { # re-calculate everytime conditions are met
# cat("dt_sub_index_ids\n");print(names(val))
    req(input$view_index == "tab.index1")
#    req(length(reactiveValuesToList(val)) <= 8 && length(reactiveValuesToList(val)) >=10)
    req(val$dt_ev_filtered, val$dt_ev_filtered, val$dt_description_clean, val$dt_desc_ev_clean)
# cat(" req satisified\n")
    # ID, sex, ..., trait1, trait2, ... index1, index2, ...
    val$dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session,
                          val$dt_ebv_filtered, val$dt_ev_filtered, val$dt_description_clean,
                          val$dt_desc_ev_clean)
    
    # ID, sex, ... index1, index2...
    val$dt_sub_index_ids <- 
      val$dt_sub_ebv_index_ids[,!names(val$dt_sub_ebv_index_ids) 
                               %in% val$dt_description_clean$column_labelling[
                                 val$dt_description_clean$classifier=="EBV"] ]
  })
  
  # takes long time to load
  renderDtTableModuleServer("index1", reactive(val$dt_sub_index_ids), T, downloadName = "index")
  
  ## INDEX STATISTICS ##
  
  ## CLUSTER ##
  votes.repub <- cluster::votes.repub
  clusteringMod("find_cl", val = reactive(NULL), dat = reactive(t(votes.repub)), cor_mat = F)
  
} # server

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
# runApp(appDir = "R")

shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020