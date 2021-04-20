# https://shiny.rstudio.com/articles/modules.html
# https://rpodcast.github.io/rsconf-2019/#28

#'Upload a table UI module
#'
#'Can accept comma separated variable file and .xlsx
#'https://shiny.rstudio.com/articles/validation.html
#'
#'@param label label to show on the UI
#'@param multiple logical, upload mutiple files together, default F
#'
#'@export
uploadTableModuleUI <- function(id, label, multiple = F) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label = label,
              multiple = multiple,
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv',
                         ".xlsx")
    )
    # textInput(ns("na.string"), "NA symbol", value = c(""))
    #,verbatimTextOutput(ns("error"), placeholder = F) # 14may2020
   #,textOutput(ns("error_message"))
  )
}

#'Upload a table server module
uploadTableModuleServer <- function(id, sheet = 1, skip = 0, what = NULL,...) {
  moduleServer(
    id,
    function(input, output, session) {

      userFile <- reactive({
        # First check if a file does exist and if not show "Please upload a file" in the UI
        # This text will be displayed in each results tab if a data set is missing
        # validate(
        #   need(input$file != "", "Please upload a file")
        # )
        req(input$file) # 19April2021 mute the warning

        # similar as above, check the file format
        validate(
          need(grepl("xls|csv|txt|RData", input$file$name),
               "Your input file is in an illegal format. Please input a comma separated variable file
           , .xlsx format, or .RData format.")
        )
        return(input$file) # this line won't run if the previous validate() didn't pass
      })

      # pars <- reactiveValues(filee = NULL) # necessary if module has multiple reactives

      # return a data.frame or error message, depending on try() results
      # filee <-
      reactive({
      # observeEvent(userFile, {
        if(grepl("\\.xls", userFile()$datapath)){
print(".xlsx")
          if(!is.null(what)) {
            what[grepl("integer|double", what)] <- "numeric"
            what[grepl("POSIX", what)] <- "date"
            what[grepl("character|factor", what)] <- "text"
          }
          dat <- try(readxl::read_xlsx(userFile()$datapath, col_types = what,
                                       sheet = sheet, skip = skip, col_names = T, trim_ws = T,
                                       ...
          ))
        } else if(grepl("\\.csv|\\.txt", userFile()$datapath)) {
print(".txt or .csv")
          if(is.null(what)) what <- NA # NULL will skip the column

          dat <- try(
            read.table(userFile()$datapath, colClasses = what,
                       header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                       quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                       strip.white = T,
                       # na.string = c(""),
                       # input$na.string, # by default both "" and "NA" are NAs besides other NA strings
                       ...)
          )
        # } #else if(grepl("\\.csv", userFile()$datapath)) {
# print(".csv")
#           dat <- try(
#             read.csv(userFile()$datapath,
#                        header = T, fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
#                        # , quote = "", na.string = c(""),
                       # input$na.string, # by default both "" and "NA" are NAs besides other NA strings
#                        ...)
#           )
        } else if(grepl("\\.RData", userFile()$datapath)) {
print(".RData")
          dat <- try(readRDS(userFile()$datapath, ...))
        }

        validate(need(class(dat)!="try-error", attr(dat, "condition")$message))

        # nothing below will execute if doesn't pass validate() above
      #  pars$filee <- dat
       return(dat)
      }) # reactive

      # # show error message immediately below the uploading box
      # error_message <- renderText({
      #   validate(need(
      #     class(pars$filee)!="try-error",
      #     attr(pars$filee, "condition")$message))
      # })

      # return(reactive({pars$filee}))
    })}

#' Download button UI
#'
#' @param id shiny id, the same as output$ object in its server function
#' @param label label to show on the button
#'
#' @return a download button
downloadModuleUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  downloadButton(ns("download_table"), label = label)
}

#' Download button server
#'
#' @param id shiny object id
#' @param downloadName the name of the file to save
#' @param df the data.frame or tibble to download
#' @param type string, options are "csv", "rdata". By default type = "csv"
#'
#' @return a .csv file
downloadModuleServer <- function(id, downloadName = "test_download", df,
                                 row.names = F, type = "csv") {
  moduleServer(
    id,
    function(input, output, session) {
     if(type == "csv") {
        output$download_table <- downloadHandler(
          filename = paste0(downloadName, "-", Sys.Date(), ".csv"),
          content = function(file) {
cat("csv"); print(dim(df))
            return(
              write.csv(df, file, row.names = row.names, quote = F)) #, fileEncoding = "UTF-8-BOM"))
          }
        )
     } else if (type == "rdata") {
       output$download_table <- downloadHandler(
         filename = paste0(downloadName, "-", Sys.Date(), ".RData"),
         content = function(file) {
cat("rdata"); print(dim(df))
           return(
           saveRDS(df, file))
         }
       )
     }

    #  } else if (type == "xlsx") {
      #   output$download_table = downloadHandler(
      #     filename = paste0(downloadName, "-", Sys.Date(), ".xlsx"),
      #     content = function(file) {
      #       write. # need package to write xlsx
      #     }
      #   )
      # }
    })}

#'Render a \object{DataTable} UI object.
renderDtTableModuleUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  tagList(
    #div(DT::dataTableOutput(ns("table"))) #style = 'overflow-x: scroll',
    div(DT::DTOutput(ns("table"))),
    downloadModuleUI(ns("download_1"), label)
  )
}

#'Render a \object{DataTable} object.
#'
#'@param dat a reactive function. The data table to render
#'@param extensions e.g. "FixedColumns" will "freezes" in place
#'       the left most columns in a scrolling DataTable. For more
#'       see https://datatables.net/extensions/index
#'@param fixedHeader boolean var. The header "freeze" while scrolling
#'@param leftColumns number of columns from the left to "freeze"
#'@param scrollX horizontal scrolling. Can be boolen or width,
#'       e.g. \code{False} means cannot scroll, or \code{"200px"}
#'       means fixed width and can scroll
#'@param digits a reactive function taken from \code{input$signdigit}
#'@param colourcode a reactive function, logical, whether to show heatmap
#'       colours in the table
#'@param dom string, the button type
#'@param buttons shiny button to show/hide columns
#'
#'@references  https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
#' basic https://shiny.rstudio.com/gallery/datatables-options.html
#' filter https://yihui.shinyapps.io/DT-info/
#' interactive https://laustep.github.io/stlahblog/posts/DTcallbacks.html
renderDtTableModuleServer <- function(id, dat = reactive(), rownames = F,
                                     extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                                     fixedHeader = F, leftColumns = 0, # fixed left most column
                                     scrollX = F,
                                     digits = reactive(3),
                                     colourcode = reactive(FALSE),
                                     dom = "Bfrtip", buttons = I('colvis'),
                                     downloadName = "test_download", row.names = F, type = "csv",
                                     editable = T, colfilter = "top") {
  moduleServer(
    id,
    function(input, output, session){

  output$table <- DT::renderDT({
    withProgress(
      message = 'Loading data', value = 1,
      {
        req(!is.null(dat())) # 14oct2020

        columns <- which(sapply(data.frame(dat()), class) %in% c("numeric"))
        optionss = list(
          # searching = T,
          fixedHeader = fixedHeader,
          fixedColumns = list(leftColumns = leftColumns, # 1 column on the left most
                              rightColumns = 0,    # no column on the right most
                              fluidColumns = TRUE, # flexible column width
                              scrollX = scrollX)
          # stateSave = T # 8Sept2020
        )
        if("Buttons" %in% extensions) {
          optionss$dom <- dom
          optionss$buttons <- buttons
        }

        dt_output <-
          DT::datatable(dat(), rownames = rownames,
                        extensions = extensions,
                        filter = colfilter, # col filter
                        #selection = list(mode = "multiple", target = "row+column"),#"multiple",
                        editable = editable,
                        options = optionss
        )

        if(length(columns) > 0) { # 25nov2020
         dt_output <- DT::formatRound(dt_output, columns = columns, digits = digits())
        }

        if(colourcode()) { # heatmap color coding
          # sanity check
          if(!typeof(as.matrix(dat())) %in% c("character", "factor") && # numeric matrix
             # diff(dim(dat()))==0 &&                                     # square matrix,
             # ebv~index cor can't display color because not squared
             (nrow(dat())>2 || ncol(dat())>2)) {                        # dimention > 2

            breaks <- min(max(length(unique(dat()))-1, 2), 9)
            cuts <- findCuts(dat(), breaks = breaks)

            colors <- findColors(dat(), n = breaks + 1) #c(-.0000000001, 0),
            # print("module")
            # print(breaks)
            # print(cuts)
            # print(colors)
            # if(length(colors)!=length(cuts)+1) {stop(cat(length(cuts), "", length(colors)))}

            dt_output <- dt_output %>%
              formatStyle(columns = columns,
                          backgroundColor = styleInterval(cuts = cuts, values = colors)
              )
          } # if
        } # if colourcode
        return(dt_output)
      }) # withProgress
    return(dt_output)
  }, server = T) #, options = list(stateSave =T)) #, 8sept2020
  #filter = "top") # renderDT/DT::renderDataTable

# observeEvent(input$table_state, { # 8sept2020 OK
#   print("input$table_state")
#   #print(input$table_rows_all[1:3])
#     print(input$table_state$columns[1:3])
# })
#
  # observeEvent(input$table_search_columns, { # 10sept2020 ok
  #   print("input$table_search_columns")
  #   print(input$table_search_columns)
  # })

# observeEvent(input$table_cell_edit, { # 8sept2020 doesn't work without editable=T
#   print("input$table_cell_edit")
# print(input$table_cell_edit[1:3])
#  # d9 <<- editData(d9, input$x9_cell_edit, 'x9', rownames = FALSE)
# })

   #isolate(
      downloadModuleServer("download_1", downloadName, dat(), row.names, type)
  # )

  })} # renderDtTableModuleServer

#'Download button UI function
downloadPlotModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("downloadPlot1"), "Download the plot")
}

#'Download plot server function
#'@param name reactive function, name to save the plot file as
#'@return \item{downloadPlot1} a downloadHandler UI
downloadPlotModuleServer <- function(name = "test_plot", plots = reactive(NULL),
                                     width = "100%", height = "800px") {
  moduleServer(
    id,
    function(input, output, session){
      
      output$downloadPlot1 <- downloadHandler(
        filename = paste0(name, "_", Sys.Date(), ".png"), # function() {paste0(name(), "_", Sys.Date(), ".png")},
        content = function(file) {
          png(file, width, height, pointsize = 14) # 908, 550
          if(class(plots())=="list" && "call" %in% names(plots())) {
            eval(plots()$call)
          } else {
            print(plots())
          }
          dev.off()
        }, contentType = "image/png"
      )
    })}


