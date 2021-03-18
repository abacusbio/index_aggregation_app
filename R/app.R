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
library(ggplot2)
library(PMA) # spc
library(gplots) # heatmap.2
library(ggrepel) # geom_text_repel
library(ggdendro)
library(factoextra) # silouette, wss

source("module_clustering.R", echo = F)
source("function_clustering.R")
source("function_clustering.R")
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
      navbarMenu("Clustering", menuName = "menu.cluster",
          # "----",
          #  "Survey Gizmo file preprocess", # section header
          tabPanel( # on the dropdown list of navbarmenu
              "Clustering analysis", value = 'tab.cluster',
              # Sidebar on the left
              sidebarLayout(
                  sidebarPanel(
                      conditionalPanel(
                          condition = "input.run_cluster == 'tab.stepn' &&
                          input.plant_app == 'tab.cluster'",
                          clusteringSidebarUI("test")
                      ),
                      width = 3), # sidebarPanel

                  # Show a plot of the generated distribution
                  mainPanel(
                      tabsetPanel(id = "run_cluster", # id can't have .
                          tabPanel("Step n: run cluster", value = "tab.stepn",
                              clusteringUI("test")     
                              )
                                    
                      ) # tabsetPanel run_cluster
                        
                  ), # mainPanel
                  fluid = T) # sidebarLayout fluid = F doesn't work here
          ) # tabPanel Clustering analysis
      ) # navbarMenu Clustering
  ) # nevbarPage
) # ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## OPTIONS ###
    # allow file sizes up to 100MB
    options(shiny.maxRequestSize = 100 * 1024 ^ 2  , shiny.trace = F # F
            , shiny.error = NULL #browser
    )
    
    data("votes.repub")
    clusteringMod("test", val = reactive(NULL), dat = reactive(t(votes.repub)), cor_mat = F)
    
}

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
# runApp(appDir = "R")

shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020