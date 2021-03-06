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
                         '.csv', ".xlsx", ".RData"))
    # textInput(ns("na.string"), "NA symbol", value = c(""))
    #,verbatimTextOutput(ns("error"), placeholder = F) # 14may2020
   #,textOutput(ns("error_message"))
  )
}

#'Upload a table server module
uploadTableModuleServer <- function(id, sheet = 1, skip = 0, what = NULL, message = "",...) {
  moduleServer(
    id,
    function(input, output, session) {

      userFile <- reactive({
        # First check if a file does exist and if not show "Please upload a file" in the UI
        # This text will be displayed in each results tab if a data set is missing
        validate(
          need(input$file != "", message) # Please upload a file
        )
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
  downloadButton(ns("download_table"), label = label, class = "btn btn-outline-primary")
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
                                 row.names = F, type = "csv", col.names = T) {
  moduleServer(
    id,
    function(input, output, session) {
     if(type == "csv") {
        output$download_table <- downloadHandler(
          filename = paste0(downloadName, "-", Sys.Date(), ".csv"),
          content = function(file) {
cat("download csv"); print(dim(df))
            return(
              write.table(df, file, sep = ",", 
                          row.names = row.names, quote = F, col.names = col.names)) #, fileEncoding = "UTF-8-BOM"))
          }
        )
     } else if (type == "rdata") {
       output$download_table <- downloadHandler(
         filename = paste0(downloadName, "-", Sys.Date(), ".RData"),
         content = function(file) {
cat("download rdata"); print(dim(df))
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
    verbatimTextOutput(ns("status0")), # 7sept2021
    verbatimTextOutput(ns("status1")),
    verbatimTextOutput(ns("status2")), 
    verbatimTextOutput(ns("status3")), 
    verbatimTextOutput(ns("status4")), 
    verbatimTextOutput(ns("status5")), 
    verbatimTextOutput(ns("status6")), 
    verbatimTextOutput(ns("status7")), 
    verbatimTextOutput(ns("status8")),
    div(DT::DTOutput(ns("table"))),
    downloadModuleUI(ns("download_1"), label),
    verbatimTextOutput(ns("clientdataText")) # 26aug2021
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
#'@param option_list additional elements to append to \code{options} argument in 
#'       \link[DT]\code{datatable}
#'
#'@references  https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
#' basic https://shiny.rstudio.com/gallery/datatables-options.html
#' filter https://yihui.shinyapps.io/DT-info/
#' interactive https://laustep.github.io/stlahblog/posts/DTcallbacks.html
#' disable search bar: http://legacy.datatables.net/usage/options
renderDtTableModuleServer <- function(id, dat = reactive(), rownames = F,
                                      extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                                      fixedHeader = F, leftColumns = 0, # fixed left most column
                                      scrollX = F,
                                      digits = reactive(3),
                                      colourcode = reactive(FALSE),
                                      dom = "Bfrtip", buttons = I('colvis'),
                                      downloadName = "test_download", row.names = F, type = "csv",
                                      editable = F, colfilter = "top",
                                      option_list = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session){
      # cat("renderDtTableModuleServer\n")
      output$table <- DT::renderDT({
        output$status0 <- renderText({paste0("0/10, start DT::renderDT at ", t)})
        t <- Sys.time()
        
        dat <- debounce(dat, millis = 1000, priority = 100, domain = getDefaultReactiveDomain())
        digits <- debounce(digits, 1000, priority = 99)
        colourcode <- debounce(colourcode, 1000, 98)
        output$status1 <- renderText({
          paste0("1/10, debounced at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
        # cat(" domain:");print(getDefaultReactiveDomain()) # different in different module calls
        
        test <- try(withProgress(
          message = 'Loading table...', min = 0, max = 1, value = 0,
          {
            req(!is.null(dat())) # 14oct2020
            #  incProgress(0.1, detail = paste0("1/10, dat has", nrow(dat()), "rows"))
            output$status2 <- renderText({
              paste0("2/10, dat has ", nrow(dat()), " rows at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            ### client side error handling 26aug2021 ###
            # Store in a convenience variable
            cdata <- session$clientData
            
            # Values from cdata returned as text
            output$clientdataText <- renderText({
              cnames <- names(cdata)
              
              allvalues <- lapply(cnames, function(name) {
                paste(name, cdata[[name]], sep = " = ")
              })
              paste(allvalues, collapse = "\n")
            })
            ### end client side error handling ###
            # incProgress(0.1, detail = "2/10, clientData generated")
            output$status3 <- renderText({
              paste0("3/10, clientData generated at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            
            columns <- which(sapply(data.frame(dat()), class) %in% c("numeric", "integer", "double"))
            # cat("renderDtTableModuleServer\n dat():");print(str(dat()))
            # 20july2021 test Ajax error rsconnect https://github.com/rstudio/DT/issues/266
            # each column inside a data.fram has to be a vector instead of an array(>=1 dimensions)
            datt <- dat()
            for(i in columns) datt[[i]] <- getFunction(paste0("as.", class(datt[[i]])))(datt[[i]])
            # cat(" datt:\n");print(str(datt))
            # incProgress(0.1, "3/10, changed column classes.")
            output$status4 <- renderText({
              paste0("4/10, changed column classes at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            
            downloadModuleServer("download_1", downloadName, datt, row.names, type)
            
            optionss = list(
              # searching = T,
              fixedHeader = fixedHeader,
              fixedColumns = list(leftColumns = leftColumns, # 1 column on the left most
                                  rightColumns = 0,    # no column on the right most
                                  fluidColumns = TRUE, # flexible column width
                                  scrollX = scrollX)
              # stateSave = T # 8Sept2020
              # 5aug2021 test Ajax error in rsconnect https://rstudio.github.io/DT/server.html
              # doesn't work
              # ajax = list(serverSide = TRUE, processing = TRUE,
              # url = DT::dataTableAjax(session, datt, outputId = id))
            )
            if("Buttons" %in% extensions) {
              optionss$dom <- dom
              optionss$buttons <- buttons
            }
            
            optionss <- append(optionss, option_list)
            # incProgress(0.1, detail = "4/10, options finished.")
            output$status5 <- renderText({
              paste0("5/10, options finished at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            
            dt_output <-
              DT::datatable(datt, rownames = rownames,
                            extensions = extensions,
                            filter = colfilter, # col filter
                            #selection = list(mode = "multiple", target = "row+column"),#"multiple",
                            editable = editable,
                            options = optionss, ... # class = "table-primary"
              )
            # incProgress(0.1, "5/10, dt_output generated.")
            output$statu6 <- renderText({
              paste0("6/10, dt_output generated at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            
            columns <- which(sapply(data.frame(datt), class) %in% c("numeric", "double"))
            if(length(columns) > 0) { # 25nov2020
              dt_output <- DT::formatRound(dt_output, columns = columns, digits = digits())
            }
            # incProgress(0.1, "6/10, formatted dt_output.")
            output$status7 <- renderText({
              paste0("7/10, formatted dt_output at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
            
            if(colourcode()) { # heatmap color coding
              # sanity check
              if(!typeof(as.matrix(datt)) %in% c("character", "factor") && # numeric matrix
                 # diff(dim(dat()))==0 &&                                     # square matrix,
                 # ebv~index cor can't display color because not squared
                 (nrow(datt)>2 || ncol(datt)>2)) {                        # dimention > 2
                
                breaks <- min(max(length(unique(datt))-1, 2), 9)
                cuts <- findCuts(datt, breaks = breaks)
                
                colors <- findColors(datt, n = breaks + 1) #c(-.0000000001, 0),
                # print("module")
                # print(breaks)
                # print(cuts)
                # print(colors)
                # if(length(colors)!=length(cuts)+1) {stop(cat(length(cuts), "", length(colors)))}
                
                dt_output <- dt_output %>%
                  DT::formatStyle(columns = columns,
                                  backgroundColor = styleInterval(cuts = cuts, values = colors)
                  )
              } # if
            } # if colourcode
            # incProgress(0.4, "10/10, cell background colored.", )
            output$status8 <- renderText({
              paste0("10/10, cell background colored at ", Sys.time(), " diff t: ", round(Sys.time()-t, 4))})
          })) # withProgress
        
        if(class(test)[1]=="try-error") {
          print(test)
        } else print(class(test))
        
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
  })} # renderDtTableModuleServer

#'Render a \object{table} UI object.
renderTableModuleUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  tagList(
    div(shiny::tableOutput(ns("table"))), # 5aug2021
    downloadModuleUI(ns("download_1"), label)
  )
}

#' Same as above but use \code[shiny]{renderTable} instead of \code{DT} package
renderTableModuleServer <- function(id, dat = reactive(), rownames = F,
                                      extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                                      fixedHeader = F, leftColumns = 0, # fixed left most column
                                      scrollX = F,
                                      digits = 3,
                                      colourcode = reactive(FALSE),
                                      dom = "Bfrtip", buttons = I('colvis'),
                                      downloadName = "test_download", row.names = F, type = "csv",
                                      editable = T, colfilter = "top",
                                      option_list = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session){
     output$table <- shiny::renderTable({
# cat("renderTableMod, digits:", digits, "\n")      
     #   withProgress(
      #    message = 'Loading table...', value = 0,
       #   {
            req(!is.null(dat())) # 14oct2020
            
            downloadModuleServer("download_1", downloadName, dat(), row.names, type)
            
            columns <- which(sapply(data.frame(dat()), class) %in% c("numeric", "integer", "double"))
            # cat("renderDtTableModuleServer\n dat():");print(str(dat()))
            # 20july2021 test Ajax error rsconnect https://github.com/rstudio/DT/issues/266
            # each column inside a data.fram has to be a vector instead of an array(>=1 dimensions)
            datt <- dat()
            for(i in columns) datt[[i]] <- getFunction(paste0("as.", class(datt[[i]])))(datt[[i]])
# cat(" datt:\n");print(str(datt))        
     #     }) # withProgress
        
        return(datt)
      }, rownames = rownames, server = T, digits = digits) #, options = list(stateSave =T)) #, 8sept2020
      #filter = "top") # renderDT/DT::renderDataTable
    })} # renderTableModuleServer

#'Render a \code[reactable]{reactable} UI object.
renderRctTableModuleUI <- function(id, label = "Download the table") {
  ns <- NS(id)
  tagList(
    #div(DT::dataTableOutput(ns("table"))) #style = 'overflow-x: scroll',
    div(reactable::reactableOutput(ns("table"))),
    downloadButton(ns("download_table"), label, class = "btn btn-outline-primary")
  )
}

#'Render a \code[reactable]{reactable} object.
#'
#'@param dat a reactive function. The data table to render
#'@param scrollX horizontal scrolling. Can be boolen or width,
#'       e.g. \code{False} means cannot scroll, or \code{"200px"}
#'       means fixed width and can scroll
#'@param digits a reactive function taken from \code{input$signdigit}
#'@param colourcode a reactive function, logical, whether to show heatmap
#'       colours in the table
#'@param colfilter a logical value, show or hide column filter
#'
#'@references  https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
#' basic https://shiny.rstudio.com/gallery/datatables-options.html
#' filter https://yihui.shinyapps.io/DT-info/
#' interactive https://laustep.github.io/stlahblog/posts/DTcallbacks.html
renderRctTableModuleServer <- function(id, dat = reactive(NULL), rownames = F,
                                       leftColumns = 0, # fixed left most column
                                       scrollX = F,
                                       digits = reactive(3),
                                       colourcode = reactive(FALSE),
                                       downloadName = "test_download", row.names = F, type = "csv",
                                       colfilter = F, searchable = T, ...){
  moduleServer(
    id,
    function(input, output, session){
      
  output$table <- reactable::renderReactable({
    #   withProgress(
    #     message = 'Loading data', value = 1,
    #    {
    req(length(dat()) > 0)
# cat("renderRctTableModuleServer, downloadName:", downloadName, "\n")        
    # download module
    output$download_table <- downloadHandler( # 12june2020
      filename = paste0(downloadName, "-", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(dat(), file, row.names = row.names)
      }
    )
# cat("renderRctTabelModuleServer\n")    
    # make sure col classes are correct
    columns <- which(sapply(data.frame(dat()), class) %in% c("numeric", "integer", "double"))
    # 20july2021 test Ajax error rsconnect https://github.com/rstudio/DT/issues/266
    # each column inside a data.fram has to be a vector instead of an array(>=1 dimensions)
    if("data.frame" %in% class(dat())) {
      datt <- dat()
    } else {datt <- as.data.frame(dat(), stringsAsFactors = F)}
    
    for(i in columns) {
      # print(class(datt[[i]]))
      datt[[i]] <- getFunction(paste0("as.", class(datt[[i]])))(datt[[i]])
    }
# cat(" datt:\n");print(str(datt))
    # initiate colDef
    allColDefs <- list()
    
    ## colDef for 1st column
    if(leftColumns==1) {
      firstColdefs <- list( colDef(
        style = list(position = "sticky", left = 0, zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, zIndex = 1)
      ))
      names(firstColdefs) <- names(datt)[1]
      allColDefs <- append(allColDefs, firstColdefs  )
    }
    
    ## colDef for numeric columns  
    columns <- which(sapply(data.frame(datt), class) %in% c("numeric", "double"))
    # cat(" columns:");print(columns)
    if(length(columns > 0)) { # 23dec2020 avoid error
      
      numColdefs <- list( colDef(format = colFormat(digits = digits())))
      numColdefs <- rep(numColdefs,length(columns))
      names(numColdefs) <- names(datt)[columns]
      allColDefs <- append(allColDefs, numColdefs)
    }
    # cat(" numcoldefs name:");print(names(numColdefs));cat(" allColDefs name:");print(names(allColDefs))
    dt <- reactable::reactable(
      datt,
      height = ifelse(class(scrollX)=="character", scrollX, "auto"),
      filterable = colfilter, searchable = searchable, rownames = rownames,
      showPageSizeOptions = ifelse(class(scrollX)=="character", F, !scrollX), 
      striped = T, highlight = T, resizable = T, 
      theme = reactableTheme(backgroundColor = "#ffffff00", # transparent
                             headerStyle = list(color = "white", background = "#78c2ad")),
      # defaultColDef = 
      columns = if(length(allColDefs)==0) {NULL} else {allColDefs}
    ) 
    
    if(colourcode()) { # This can't force ranging -1, 1 now. Cheryl doesn't like it
      # sanity check
      if(!typeof(as.matrix(datt)) %in% c("character", "factor", "logical") && # numeric matrix
         # diff(dim(dat()))==0 &&                                     # square matrix,
         # ebv~index cor can't display color because not squared
         (nrow(datt)>2 || ncol(datt)>2)) {                        # dimention > 2
        
        # breaks <- min(max(length(unique(dat()))-1, 2), 9)
        # cuts <- findCuts(dat(), breaks = breaks)
        # colors <- findColors(dat(), n = breaks + 1, cuts = cuts) #c(-.0000000001, 0),
        # sorted <- sort(dat())
        
        # make color palette function
        makeColPal <- function(colors, bias = 1) {
          get_color <- colorRamp(colors, bias = bias)
          function(x) rgb(get_color(x), maxColorValue = 255)
        }
        
        if(all(datt<=0)) {
          c("steelblue3", "white")
        } else if(all(datt>=0)) {
          colors <- c("white", "coral")
        } else {
          colors <- c("steelblue3", "coral")
        }
        
        heatmapcol <- makeColPal(colors, bias = 2)
        
        numColdefs <- list(colDef(format = colFormat(digits = digits()),
                                  style = function(value) {
                                    # cat("  value", value, "\n")
                                    value <- ifelse(value < 0, 0, value)
                                    value <- ifelse(value > 1, 1, value)
                                    return(list(background = heatmapcol(value)))
                                  }))
        numColdefs <- rep(numColdefs, ncol(datt))
        names(numColdefs) <- names(datt)
        
        dt <- reactable::reactable(
          datt,
          height = ifelse(class(scrollX)=="character", scrollX, "auto"),
          filterable = colfilter, searchable = searchable, rownames = rownames,
          showPageSizeOptions = ifelse(class(scrollX)=="character", F, !scrollX),
          striped = T, highlight = T, resizable = T,
          theme = reactableTheme(backgroundColor = "#ffffff00", # transparent
                                 headerStyle = list(color = "white", background = "#0a8a98")),
          columns =  numColdefs
        )
      } # if all cols are numeric
    } # if colourcode
    #      }) # withProgress
    return(dt)
  }) # renderReactable
})} # renderRctModuleServer

#'Download button UI function
downloadPlotModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("downloadPlot1"), "Download the plot", class = "btn btn-outline-primary")
}

#'Download plot server function
#'@param name a string, name to save the plot file as
#'@return \item{downloadPlot1} a downloadHandler UI
downloadPlotModuleServer <- function(id, name = "test_plot", plots = NULL,
                                     width = reactive(800), height = 800) {
  moduleServer(
    id,
    function(input, output, session){
# cat("downloadPlotModuleServer width:", width(), ", height:", height, "\n")
      output$downloadPlot1 <- downloadHandler(
        filename = function() {paste0(name, "_", Sys.Date(), ".png")},
        content = function(file) {
          
          png(file, width(), height, "px", pointsize = 14)
          if(class(plots)=="list" && "call" %in% names(plots)) {
            eval(plots$call)
          } else {
            print(plots)
          }
          
          dev.off()
        }, contentType = "image/png"
      )
    })}


