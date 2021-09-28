#' Render a \object{DataTable} UI object.
#' @param defaultName a string. The default file name to be saved after clicking 'Rename and save'
#'        button.
#' @references
#' https://github.com/radiant-rstats/radiant.data/blob/master/inst/app/tools/data/view_ui.R
#' https://fontawesome.com/icons?d=gallery
dataViewerModuleSidebarUI <- function(id, defaultName = "") {
  ns <- NS(id)
  tagList(
    #  tags$td(
    shinyjs::disabled(
      div(id = ns("bttn"),
          actionButton(ns("view_store"), "Filter and save", icon = icon("pen"),
                       class = "btn-success") #style = "padding-top:20px;")),
    )),
    downloadModuleUI(ns("download_1")),
    downloadModuleUI(ns("download_2"), "Download the filter records"),
    #， help_and_report(
    #   "View", "view",
    #   inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/view.md")) %>%
    #   gsub("`", "", .),
    #   lic = "by-sa"
    # )
    br(),br(),
    h4("Variable display controller"),
    wellPanel(
      #   actionLink(ns("view_clear"), "Clear settings", icon = icon("refresh"), style = "color:black"),
      selectInput(ns("view_vars"), "Select variables to show:", "", "",
                  multiple = T, selectize = F, size = 15),
      # sustitute decimal places with digits below
      numericInput(ns("view_dec"), "Decimals:", value = 2, min = 0),
      #tags$table(
      #  tags$td(
      textInput(ns("view_name"), "Rename new data as:", defaultName,
                          placeholder = "Provide data name")#),
    )#, # wellPanel
  )
}

dataViewerModuleTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    # span(uiOutput(ns("stefan_filter_error_message")), style = "color:salmon"),
    div(DT::DTOutput(ns("dataviewer")))
  )
}

#' Render a \object{DataTable} object.
#'
#' @param id The id string to be namespaced.
#' @param dat A reactive function. The data table to render.
#' @param val A reactive value. Used to store filtered data frame.
#' @param filter_dat_name A string. The name to give \object{val} to store the filtered data.frame.
#' @param filter_cols A reactive function. The vector of column names in StefanFilterModUI
#'        e.g. \object{input$filter_col}
#' @param filter_levels A reactive function. The list of unique column levels in stefanFilterModUI
#'        e.g. \object{input$data_filters}
#' @param apply A reactive function. The apply filter button in stefanFilterModuleUI.
#'
#' @details Modules can't read input directly. Have to create arguments for input and set them as
#'          reactive functions.
#' @references https://rstudio.github.io/DT/options.html
#' https://github.com/radiant-rstats/radiant.data/blob/master/inst/app/tools/data/view_ui.R
#' in the future https://blog.rstudio.com/2018/03/29/dt-0-4/
dataViewerModuleServer <- function(id, datt = reactive(NULL), val,
                                   filter_dat_name = "test_filtered",
                                   filter_cols = reactive(NULL),
                                   filter_levels = reactive(NULL),
                                   na_include = reactive(F), na_to_0 = reactive(F), # 6april2021
                                   apply = reactive(NULL) # 14june2021
                                   # vars = reactive(NULL), download = T
                                   ) {
  moduleServer(
    id,
    function(input, output, session){
      
      datt <- debounce(datt, millis = 1000, priority = 100, domain = getDefaultReactiveDomain())
      filter_cols <- debounce(filter_cols, 1000, 99)
      filter_levels <- debounce(filter_levels, 1000, 98)
      na_include <- debounce(na_include, 1000, 97)
      na_to_0 <- debounce(na_to_0, 1000, 96)
      apply <- debounce(apply, 1000, 95)
      
      # r_state <- reactiveValues(view_vars = NULL, dataviewer_state = list(),
      #                           dataviewer_search_columns = NULL)
      
      ## col var selection panel at sidebar
      observeEvent(datt(), {
        req(!is.null(datt()))

        # if(is.null(vars())) {
          vars <- names(datt())  # varnames() # 9sept2020
        # } else { vars <- vars()}

        updateSelectInput(session,
            "view_vars",  # use session$ns() to get inside id being recognized
            choices = vars,
            selected = vars #state_multiple("view_vars", vars, vars),
           )

			
      }) # ui_view_vars
      
      # ## store changes made by the user
      # observeEvent(input$dataviewer_search_columns, { # user searches in column filters
      #   r_state$dataviewer_search_columns <- input$dataviewer_search_columns
      # })
      # #
      # # # takes forever to load column by column
      # # # each column makes a DT::renderDT run once...
      # # # $columns[[129]]
      # # # $columns[[129]]$visible
      # # # [1] TRUE
      # # #
      # # # $columns[[129]]$search
      # # # $columns[[129]]$search$search
      # # # [1] ""
      # # #
      # # # $columns[[129]]$search$smart
      # # # [1] TRUE
      # # #
      # # # $columns[[129]]$search$regex
      # # # [1] FALSE
      # # #
      # # # $columns[[129]]$search$caseInsensitive
      # # # [1] TRUE
      # #       observeEvent(input$dataviewer_state, { # user does anything to the table?
      # # print("dataviewer_state"); print(head(input$dataviewer_state)        )
      # #         r_state$dataviewer_state <<-
      # #           if (is.null(input$dataviewer_state)) list() else input$dataviewer_state
      # #       })
      # #
      # observeEvent(input$view_vars, {
      #   if (length(r_state$view_vars) > 0) {
      #     r_state$dataviewer_state <<- list()
      #     r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
      #   }
      #   r_state$view_vars <<- input$view_vars
      # })
      #
      # # observeEvent(input$view_clear, { # not sure what view_clear does
      # #   r_state$dataviewer_state <<- list()
      # #   r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
      # #   r_state$view_vars <<- input$view_vars
      # #   #updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE) # Filter data check box
      # # })
      
      tmp <- reactiveValues(dat = NULL, stefan_filter = NULL)
      observeEvent(apply(), { # have to use () otherwise it doesn't react
# cat("dataViewerModuleServer\n observe apply:");print(apply())
# cat(" filter_cols:");print(filter_cols());cat(" is.null(filter_cols)", is.null(filter_cols()), "\n")
        if (!is.null(filter_cols())) { # filter_cols exists
          dat_new <- datt()
          
          for(i in 1:length(filter_cols())) {
            # print(filter_cols()[ i ]) # print(filter_levels()[[ i ]])
            dat_new <- filter_at(dat_new, vars(filter_cols()[ i ]),
                                 any_vars(. %in% filter_levels()[[ i ]]))
          }
          # print("dim data_new") # print(dim(dat_new))
          tmp$dat <- dat_new                                   # save filtered data frame
          tmp$stefan_filter <- data.frame(var = filter_cols(), # save filter records 10feb2021
                                          level = sapply(filter_levels(), paste0, collapse = ";"))
          
        } else {                      # filter_cols are cleared 14june2021
          tmp$dat <- datt()
          tmp$stefan_filter <- data.frame(var = "", level = "")
        }
      })
      
      ## datatable on tabPanel
      output$dataviewer <- DT::renderDT({
        withProgress(
          message = "Loading table...", value = 1,
          {
            req(!is.null(datt()), input$view_vars)
            # print("dataViewerModuleServer")
            
            ## next line causes strange bootstrap issue https://github.com/ramnathv/htmlwidgets/issues/281
            #  input$view_clear
            
            if(is.null(tmp$dat)) { # user didn't apply stefan filter
              dat <- datt()
            } else {
              # print("use tmp$dat")
              dat <- tmp$dat
            }
            
            # 20july2021 test Ajax error rsconnect https://github.com/rstudio/DT/issues/266
            # each column inside a data.fram has to be a vector instead of an array(>=1 dimensions)
            # 21july2021 this is not the reason of Ajax error in rsconnect
            columns <- which(sapply(data.frame(dat), class) %in% c("numeric", "integer", "double"))
# cat("dataViewerModuleServer\n");print(str(dat));cat(" columns:");print(columns)
            for(i in columns) dat[[i]] <- getFunction(paste0("as.", class(dat[[i]])))(dat[[i]])
# cat(" dat after:\n");print(str(dat))            
            dat <- select_at(dat, vars(input$view_vars)) # filter col var to show
            
            #         # below will make DT::renderDT run for each column, too
            #         search <- input$dataviewer_state$search$search # r_state$dataviewer_state$search$search
            #         if (is.null(search)) search <- ""
            # print("search");print(search)
            
            fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top") # filter box arg
            
            ## factor col that has more than 1K levels conver to character
            ## here we only have the 1st col formmated as factor
            isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
            if (sum(isBigFct) > 0) {
              dat[, isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(as.character)
            }
            
            ## for rounding
            isInt <- sapply(dat, function(x) is.integer(x))
            isDbl <- sapply(dat, is.double)
            dec <- input$view_dec %>% {ifelse(length(.)==0 || . < 0, 3, round(., 0))}
            
            dt_output <- DT::datatable(
              dat,
              filter = fbox,
              selection = "none", # initially no filter applies
              rownames = FALSE,
              ## must use fillContainer = FALSE to address
              ## see https://github.com/rstudio/DT/issues/367
              ## https://github.com/rstudio/DT/issues/379
              fillContainer = FALSE,
              ## only works with client-side processing
              extension = "KeyTable",
              escape = FALSE,
              editable = TRUE,
              style = "bootstrap", #class = "table-primary", 
              options = list(
                stateSave = TRUE, ## maintains state
                # searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)), #?
                search = list(
                  regex = TRUE, # allows regular expression
                  smart = T), # only server = T https://blog.rstudio.com/2018/03/29/dt-0-4/
                # search = search
                # order = {
                #   if (is.null(r_state$dataviewer_state$order)) {
                #     list()
                #   } else {
                #     r_state$dataviewer_state$order
                #   }
                # },
                columnDefs = list(
                  list(orderSequence = c("desc", "asc"), targets = "_all"),
                  list(className = "dt-center", targets = "_all")
                ),
                autoWidth = TRUE,
                processing = isTRUE(fbox == "none") , # ?
                pageLength = 10,
                # {
                # if (is.null(r_state$dataviewer_state$length)) 15 else r_state$dataviewer_state$length
                # },
                lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
              ), # options
              ## https://github.com/rstudio/DT/issues/146#issuecomment-534319155
              callback = DT::JS('$(window).on("unload", function() { table.state.clear(); })')
            ) %>%
              {if (sum(isDbl) > 0) DT::formatRound(., names(isDbl)[isDbl], dec) else .} %>%
              {if (sum(isInt) > 0) DT::formatRound(., names(isInt)[isInt], 0) else .}
          }) # withProgress
        
        return(dt_output)
      }, server = T # server-side processing, defaut is TRUE
      ) # DT::renderDT
      
      observeEvent(input$dataviewer_rows_all, { # when datatable has >0 rows, enable filter and save button
        if(!is.null(input$dataviewer_rows_all)) shinyjs::enable(id = "bttn")
      })
      
      ## store new name and download
      observeEvent(input$view_store, {
        req(input$view_name, val, !is.null(datt()))
        
        if(is.null(tmp$dat)) { # user didn't apply stefan filter
          dat <- datt()
        } else {
          #    print("use tmp$dat")
          dat <- tmp$dat
        }
        
        # data_filter <- if (input$show_filter) input$data_filter else ""
        out <- dat[input$dataviewer_rows_all, input$view_vars]
        
        if(na_include()) { # 6april2020
          tmp$na <- dat[,sapply(dat, class) %in% c("numeric", "double", "float", "integer")]
          idx <- which(rowSums(is.na(tmp$na)) > 0)
          na_rows <- dat[idx,]
          
          if(na_to_0()) { na_rows[is.na(na_rows)] <- 0 }
          
          out <- rbind(na_rows, out)
        }
        
        val[[filter_dat_name]] <- out
        # cat(" val[[filter_dat_name]] dim "); # debug
        # print(dim(val[[filter_dat_name]]));
        # print(table(sapply(val[[filter_dat_name]], class)))
        # if(download) {
        downloadName <- gsub(" ", "_", input$view_name)
        downloadModuleServer("download_1", downloadName = downloadName,
                             df = val[[filter_dat_name]], type = "csv")
        # }
        
        # user input filters
        dat_filter <- data.frame(var = input$view_vars,
                                 level = input$dataviewer_search_columns) %>%
          dplyr::filter(level!="")
        
        if(!is.null(tmp$stefan_filter)) {
          dat_filter <- dat_filter %>% full_join(tmp$stefan_filter, by = "var")
        }
        
        downloadModuleServer("download_2", paste0(downloadName, "_filter_records"), dat_filter,
                             type = "csv")
        
        
      }) # observeEvent
    })}
