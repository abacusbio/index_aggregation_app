#' Report button UI
#'
#' @param id shiny id, the same as output$ object in its server function
#' @param label label to show on the button
#'
#' @return a download button
reportModUI <- function(id, label = "Generate report") {
  ns <- NS(id)
  downloadButton(ns("report"), label = label, class = "btn btn-outline-primary")
}

#' Report generator server
#'
#' @param id shiny object id
#' @param downloadName the name of the file to save
#' @param df the data.frame or tibble to download
#' @param type string, options are "csv", "rdata". By default type = "csv"
#'
#' @return a .csv file
reportMod <- function(id, file_name = "report_index_aggregation.doc") {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = file_name,
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(
            vector_test = input$`sumstat_ebv-vars`,
            table_test = input$`sumstat_ebv-stat_num` ,
            plot_test = input$`sumstat_ebv-hist_num`
          ) 
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file # ,
                           # params = params,
                            # envir = new.env(parent = globalenv() # use with params
                           )
        }
      )
    })}