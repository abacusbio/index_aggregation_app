# renv::settings$external.libraries("C:/Program Files/Microsoft/R Open/R-4.0.2/library")
# don't source()!!! do runApp("R")
# rsconnect::writeManifest("R")

# devtools::load_all()
# install.packages("https://cran.r-project.org/src/....tar.gz", type = "source", repos = NULL)
options(repos = c("MRO" = "https://mran.microsoft.com/snapshot/2020-07-16", # "added" = "https://mran.microsoft.com/snapshot/2019-04-15", # too old
                  "CRAN" = "https://cran.rstudio.com",
                  "added1" = "https://cran.r-project.org"))
# options("repos")
# old.packages() 
# rsconnect::appDependencies()

# options(shiny.reactlog = T) # lzhang April172020
# reactlogReset()
# Run the application 

# source("ui.R");source("server.R")
runApp(appDir = "R")

# shinyApp(ui = ui, server = server)
# reactlogShow(time = T) # 18may2020