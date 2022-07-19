packages <- c(
  "shiny",
  "DT",
  "shinyWidgets",
  "reactable",
  "dplyr",
  "purrr",
  "ggplot2",
  "pheatmap",
  "ggrepel",
  "ggdendro",
  "cluster",
  "PMA",
  "factoextra",
  "markdown", # deployment error complaining no markdown 
  "tinytex" # for pdf output report. tinytex::install_tinytex(); Sys.which('pdflatex')
)

# Suppress package load messages for shiny tests.
if (isTRUE(getOption("shiny.testmode"))) {
  suppressPackageStartupMessages({
    invisible(lapply(
      packages,
      library,
      character.only = TRUE,
      quietly = TRUE
    ))
  })
}else {
  lapply(packages, library, character.only = TRUE)
}


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
# source("module_report.R")
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
                     indexSumstatModSidebarUI("index_view")
                   ),
                   width = 4), # sidebarPanel
                 
                 mainPanel(
                   tabsetPanel(id = "view_index", # id can't have .
                               tabPanel("View index", value = "tab.index1",
                                        # renderDtTableModuleUI("index1") # too long
                                        br(),
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
             ), # tabPanel Aggregation
             
             tabPanel( # on the dropdown list of navbarmenu
               "Report", value = 'tab.report',
               # Sidebar on the left
               sidebarLayout(
                 sidebarPanel(
                   conditionalPanel(
                     condition = "input.plant_app == 'tab.report'",
                     radioButtons("report_format", "Document format", c("HTML", "Word"),# "PDF"),
                                  inline = TRUE)
                   ), width = 4),
                 
                 mainPanel(
                   div(p("Some graphs or tables can take a long time to generate. Please be patient."),
                       class = "text-info"),
                   # reportModUI("report")
                   downloadButton("report", "Generate report", class = "btn btn-outline-primary")
                 ),
                 fluid = T) # sidebarLayout fluid = F doesn't work here
             ) # tabPanel Report
  ) # nevbarPage
) # ui
