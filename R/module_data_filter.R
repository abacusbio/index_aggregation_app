#' Module UI function to create dynamic filter
#' @author Stefan Meyer original function
#' @author Luna Zhang modulize
stefanFilterModUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("String variable level filter controller"),
    wellPanel(
      actionButton(ns("clear"), "Clear selections", icon("eraser"), 
                   class = "btn btn-outline-primary"),
      uiOutput(ns("filter_col_UI")), # Stefan's filter Col selector
      uiOutput(ns("data_filters")), # Stefan's filter level selector for each col selected above
      actionButton(ns("stefan_button"), label = "Apply filter", icon = icon("hand-point-right")
                   , class = "btn btn-outline-primary")
    )
  )
}

#' Module server function to create dynamic filter
#'
#' @param dt, reactive function of a data frame to filter
#'
#' @return the 1st UI is \item{"filter_col_UI"} that allows users to choose columns to filter for
#' @return the 2nd UI is \item{"data_filters"} that shows the unique levels of the columns users
#'         chose in \item{"filter_col_UI"}
stefanFilterMod <- function(id, dt = reactive()) {
  moduleServer(
    id,
    function(input, output, session){
      
      ## Function to render UI element to select columns to filter data on upon which further UI
      ##  elements for each selected data filter are generated
      
      # Render UI (id: filter_col_UI) to selected columns from catch_effort_t to filter data
      # This function calls another function ('get_column_labels' stored in
      # www/functions/retrieve_data_and_model_info/get_column_labels.R) to retrieve all available
      # column labels from the 'catch_effort_t' table
      # Call renderUI function and store as output with id 'filter_col_UI'
      output[['filter_col_UI']] <- renderUI({
        req(dt)
        # only filter string variables
        # numeric variables can be filtered in DT::datatable or DT::DT slider bars
        string_idx <- which(sapply(dt(), class) %in% c("factor", "character"))
        
        # UI element is a pickerInput widget with inputId: 'filter_col', and choices (i.e. column
        # labels) come from 'catch_effort_t' table, and 'fishing_year' is selected by default
        # https://shiny.rstudio.com/articles/modules.htmlusing renderUI within modules
        shinyWidgets::pickerInput(inputId = session$ns('filter_col')
                                  , label = 'Select columns to apply filter on'
                                  , choices = names(dt())[string_idx]
                                  , selected = NULL
                                  , multiple = TRUE
        )
      })
      
      observeEvent(input$clear, {
        shinyWidgets::updatePickerInput(session = session,
                                        inputId = "filter_col",
                                        selected = "") # NULL doesn't work!
      })
      
      # By applying 'create_data_filters', create a UI element (id: 'data_filters') containing each
      # single data filter (i.e. the selectInput widgets)
      # Call renderUI function and store as output with id 'data_filters'
      output[['data_filters']] <- renderUI({
        req(dt)
        
        tagList(
          
          # map the selected column labels from input[['filter_col']] create_data_filters; further
          # map unique values in each column onto create_data_filters;
          # note, to retrieve unique values in each column, the function query_unique_column_values
          # is called which takes input, output, session, and particularly input[['filter_col']]
          # (i.e. the different column labels so R knows from where to query the unique values from)
          purrr::pmap(
            list(input[['filter_col']],                                          # col_var
                 query_unique_column_values(filter_cols = input[['filter_col']], # choice
                                            df = dt()),
                 purrr::map(seq(input[['filter_col']]), session$ns) # id ==session$ns(input$filter_col)
            ),
            create_data_filters1)
        )
      }) # output[[data_filters]]
      
      # input$filter_col is the user selected vars 21sept2020
      
    })}

#' Generic sub-function to create selectInput widgets for every column choosen in
#' pickerInput('filter_col')
#' @details e.g. if 'fishing_year' and 'method' were selected in \item{input[['filter_col']]}
#'          then two selectedInput widget are displayed for both columns and with their contained
#'          values as choices.
#' and choice, which are the unique values for each column (see below)
#' @param col_var string, the column name of a data frame, used as selectInput label. e.g.
#'        \item{input[['filter_col']]}
#' @param choice string, the unique levels in a data frame column, used as selectInput choices
#' @param id, shiny namespace object, e.g. \item{session$ns(input[['filter_col']])}
#' @return a selectInput object as a filter UI
create_data_filters1 <- function(col_var, choice, id) {
  
  selectInput(inputId = id  # selected columns labels # ==col_var
              , label = col_var
              , choices = choice # unique values in each selected column, can't be factor
              , multiple = TRUE
              , selectize = TRUE
  )
}

#' Unique column value extractor
#' @description Function to query unique values from selected columns in catch_effort_t
#' filter_cols requies as input the column labels of the selected columns
#' the df can't have factor cols otherwise selectInput will have error
# 'All sub-lists in "choices" must be named.'
#' @param filter_cols string vector, the column name of the data frame
#' @param df data.frame or tibble, the data to be filtered on
#' @return a vector containing unique levels of a data frame column
query_unique_column_values <- function(filter_cols, df){
  
  # # Show spinner since querying the SQL table might take a while
  #   show_modal_spinner(
  #     spin = "cube-grid",
  #     color = "#087E8B",
  #     text = "Please wait..."
  #   )
  
  # Sub-function to SQL query the unique values in each column populated into the
  #  'query_unique_column_values' function
  # For this function populate the input 'filter_cols' into 'filter_cols_'
  create_filter_options <- function(filter_cols_){
    # as.data.frame(unique(df[['filter_cols_']]))
    out <- dplyr::select_at(df, filter_cols_) %>% sapply(unique) %>% sort(na.last = T)
    return(out)
  }
  
  # For example, a query for the column method would look as:
  # SELECT
  # DISTINCT('method')
  # FROM
  # catch_effort_t
  # ORDER BY
  # 'method';
  
  # and the output of that function would be a data frame looking like:
  #       [,1]
  # [1, ] "BLL"
  # [2, ] "SLL"
  # [3, ] "Trawl"
  # [..., ] "..."
  # Having it store as a data frame is important here so that each unique value is stored as
  # a separate value
  
  # Map column labels (filter_cols) on create_filter_options function
  # This creates a list with one item per selected column (for the filter), each being a
  # data frame as shown above. (see example below)
  # map filter_cols (defined via UI) on create_filter_options function
  filter_options_ <- purrr::map(filter_cols, create_filter_options)
  
  # Rename list items so the names match with the column labels
  names(filter_options_) <- filter_cols
  
  # For example, if the user selected 'method' and 'fishing_year' as data filters than the
  # corresponding output for 'filter_options_' would be:
  
  # $method
  #        [,1]
  # [1,] "'BLL'"
  # [2,] "'SLL'"
  # [3,] "'Trawl'"
  # [4,...] [...]
  #
  # $fishing_year
  #        [,1]
  # [1,] "'2002/2003'"
  # [2,] "'2003/2004'"
  # [3,] "'2004/2005'"
  # [4,] "'2005/2006'"
  # [5,] "'2006/2007'"
  
  # # Remove the modal spinner once done
  #   remove_modal_spinner()
  
  return(filter_options_)
} # query_unique_column_values

