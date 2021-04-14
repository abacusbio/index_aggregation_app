aggModUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  downloadButton(ns("download_table"), label = label)
}

#' Download button server
#'
#' @param id shiny object id
#' @param val a reactive value object
#' @param cl a reactive function of a list containing a cluster_obj and a vector of cluster
#'        assignments
#' @param index a reactive function of a data.frame, animal ID by indexes, e.g. val$dt_index        
#' 
#' @return a .csv file
aggMod <- function(id, val, cl = reactive(NULL), index = reactive(NULL), ) {
  moduleServer(
    id,
    function(input, output, session) {
      
    })}