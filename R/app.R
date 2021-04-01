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
source("function_clustering.R")
source("function_clustering.R")
source("function_clean.R")
source("function_copied_selindexrevamp.R")

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
           dataViewerModuleSidebarUI("srvgzm", defaultName = "filtered_survey_gizmo"),
           stefanFilterModUI("stfn_ebv"),
           span(textOutput("stefan_filter_error_message"), style = "color:salmon")
         ),
         width = 3), # sidebarPanel
       
       # Show a plot of the generated distribution
       mainPanel(
         tabsetPanel(id = "upload", # id can't have .
           tabPanel("Step 1: Upload", value = "tab.step1",
             br(),
             actionButton("demo", "Run demo data"),
             preprocessUploadModUI("step1")
           ),
           
           tabPanel("Step 2: Filter EBV", value = "tab.step2",
             br(),
             dataViewerModuleTabUI("srvgzm")
           )
                     
         ) # tabsetPanel run_cluster
       ), # mainPanel
       fluid = T) # sidebarLayout fluid = F doesn't work here
   ), # tabPanel Upload Files
   # ), # navbarMenu File upload
    
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
  
  desc_ebv <- readxl::read_xlsx("../data/description_bv.xlsx")
  desc_ev <- read.csv2("../data/description_ev.csv", sep = ",", 
                       col.names = c("column_labelling", "classifier"))
  dat_ebv <- read.csv2("../data/sire_bv.csv", sep = ",")
  dat_ev <- read.csv2("../data/sire_ev.csv", sep = ",")
  
  observeEvent(input$demo, {
    val$desc_ebv <- desc_ebv
    val$desc_ev <- desc_ev
    val$dat_ebv <- dat_ebv
    val$dat_ev <- val$dat_ev
  })
  
  ## UPLOAD DATA ##
  preprocessUploadMod("step1", val) # reactive(val))
  
  ## CLEAN DATA, change format etc ##
  dt_description_clean <- reactive({
    req(val$desc_ebv)
    
    cleanDescData(desc_ebv())
  })
  
  # try to checkbox "include missing accuracy" 22april2020
  dt_ebv_clean <- reactive({
    req(dt_description_clean, val$dat_ebv)
    
    cleanEbvData(dt_description_clean(), val$dat_ebv())
  }) # !! dt_ebv_clean is a reactiveExpr/reactive class, dt_ebv_clean() is a data.table class
  
  dt_desc_ev_clean <- reactive({
    req(val$desc_ev)
    
    val$desc_ev
  })
  
  dt_econval_clean <- reactive({ # 14may2020
    req(dt_description_clean, dt_econval)
    
    cleanEVplant(dt_description_clean(), dt_econval())
  })
  
  ## FILTER ##
  # create_data_filters(input, output, session,  reactive(val$dt_ebv_clean))
  stefanFilterMod("stfn_ebv", dt = reactive(val$dt_ebv_clean))
  
  filter_levels <- eventReactive(input$`stfn_ebv-stefan_button`, {
    # print("event reactive stefan button")
    req(val$dt_ebv_clean)
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
  
  # show and download data table
  # the updated datatable is stored in val$data_filtered
  dataViewerModuleServer("srvgzm", reactive(val$dt_ebv_clean), val,
                         filter_dat_name = "data_filtered",
                         filter_cols = reactive(input$`stfn_ebv-filter_col`),
                         filter_levels = filter_levels
  )
  
  ## CALCULATE INDEX ##
  
  ## CLUSTER ##
  votes.repub <- cluster::votes.repub
  clusteringMod("find_cl", val = reactive(NULL), dat = reactive(t(votes.repub)), cor_mat = F)
    
}

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
# runApp(appDir = "R")

shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020