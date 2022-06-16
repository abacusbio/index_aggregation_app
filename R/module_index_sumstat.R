indexSumstatModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(12, div(actionButton(ns("help_btn"), "", icon("question"), 
                                class = "btn btn-outline-info"), style = "float:right")),
    uiOutput(ns("ui_group")),
    h4("Table and graph display controller"),
    wellPanel(
      numericInput(ns("view_dec"), "Decimals:", value = 2, min = 0),
      numericInput(ns("font_size"), "Font size", 12, 1, 20, 1),
      br() 
    )
 #   actionButton(ns("create_table"), "Create table", icon("table"), class = "btn-success")
  )
}

indexSumstatModUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(div(id = ns("help_html"),
                        # htmltools::includeMarkdown("help/preprocess.Rmd")))
                        includeHTML(knitr::knit2html("help/index_viewer.Rmd", fragment.only = TRUE,
                                                     options = c("toc"))))),
    textOutput(ns("warning1")),
    downloadModuleUI(ns("dnld_index"), "Download the index table"),
    br(),br(),
    h2("Table"),
    # renderDtTableModuleUI(ns("stat_num")),
    renderRctTableModuleUI(ns("stat_num")),
    br(),br(),
    h2("Boxplot (up to 100 indexes)"),
    plotOutput(ns("boxplot"), height = "800px")
  )
}

#' @param index a reactive function. e.g. \code{val$dt_index}. an animal x index data.frame
#' @param val a reactive function. Should have 1) \code{val$dt_ev_filtered}, a data.frame of index x
#'        EV and other variables, 2) \code{val$dt_desc_ev_clean}, a data.frame of columns as
#'        column_labelling and classVar
indexSumstatMod <- function(id, index = reactive(NULL), val, val_report, report_prefix = NA,
                            ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("indexSumstatMod\n")    
      # initialize
      tempVar <- reactiveValues()
      
      observeEvent(input$help_btn, {
        if(input$help_btn %% 2 == 1){
          shinyjs::show("help_html")
        }else{
          shinyjs::hide("help_html")
        }
      })
      
      if(any(grepl("windows", Sys.getenv(), ignore.case = T))) {
        windows_env <- T
      } else { windows_env <- F }
      
      # warning messages
      output$warning1 <- renderText({
        # cat("sumstatUnivarMod"); print()
        validate(
          need(length(index()) > 0,
               paste0("Please save filtered EBV and EV tables first.")
          ))
      })
      
      # renderDtTableModuleServer("index1", reactive(val$dt_sub_index_ids), T, downloadName = "index")
      observeEvent(index(), {
# cat(" observeevent index\n")        
        req(!is.null(index()))
        
        downloadModuleServer("dnld_index", "index", 
                             data.frame(ID = rownames(index()), index()), F, "csv")
      })
      
      # Group by
      output$ui_group <- renderUI({
# cat(" ui_group\n");#cat("  val:");print(names(val))
        req(!is.null(val$dt_ev_filtered) && is.null(input$warning1) && 
              !is.null(val$dt_desc_ev_clean))

        names_classvar <- val$dt_desc_ev_clean$column_labelling[
          !val$dt_desc_ev_clean$classifier %in% c("ID", "EV")]
        names_classvar <- names_classvar[names_classvar %in% names(val$dt_ev_filtered)]

        vars_n_types <-
          rlang::set_names(paste0(names_classvar, "{", 
                                  sapply(val$dt_ev_filtered[, names_classvar], typeof), "}"))
        yvars <- grep("character|factor|logical|integer", vars_n_types, value = T)
 
        return(
          selectInput(session$ns("group_vars"), "Group by:", choices = c("", yvars),
                      selected = ""))
      })
      
      group_vars_d <- debounce(reactive(input$group_vars), 4000) # 8sept2021
      
      # sum stat
      observeEvent({
        index()
        group_vars_d() # input$group_vars
        }, {
# cat(" observe index, group_vars_d:", group_vars_d(), "\n")
        req(!is.null(index()), !is.null(val$dt_ev_filtered), length(input$group_vars) > 0)
          
        if(group_vars_d()!="") {
          group_vars <- isolate(sapply(strsplit(group_vars_d(), "\\{"), head, 1) %>% unlist())
        } else {
          group_vars <- group_vars_d()
        }
          
        index_vars <- names(index())
   
        index_t <- data.frame(t(index())) %>% # Index x animal
          mutate(Index = names(index()))

        df_num <- dplyr::select(val$dt_ev_filtered, Index, any_of(group_vars)) %>% 
          left_join(index_t, by = "Index") # Index [group_vars] animal1 animal2

        l_fun <- list("mean" = mean, "sd" = sd, "min" = min, "max" = max) # add n_obs, n_missing later
        
        ## sumstat numeric variables
        if(length(index_vars) > 0) {
# cat("index_vars exists --> stat_num lapply\n")
          if(group_vars=="") { # stats of each index
            
            if(windows_env) {
              stat_num <- lapply(index_vars, function(var_num) {
                
                stats <- dplyr::select(index(), all_of(var_num)) %>% 
                  summarise(across(all_of(var_num), l_fun, na.rm = T, .names = "{fn}")) %>%
                  mutate(variable = var_num)
                
                stats <- dplyr::select(stats, all_of(c("variable", names(l_fun))))
                
                return(stats)
              }) # variable   mean    sd   min   max
            } else {
              stat_num <- parallel::mclapply(index_vars, function(var_num) {
                
                stats <- dplyr::select(index(), all_of(var_num)) %>% 
                  summarise(across(all_of(var_num), l_fun, na.rm = T, .names = "{fn}")) %>%
                  mutate(variable = var_num)
                
                stats <- dplyr::select(stats, all_of(c("variable", names(l_fun))))
                
                return(stats)
              }, mc.cores = max(4, parallel::detectCores()-4)) # variable   mean    sd   min   max
            }
            
           } else {                # stats of all indexes in a group level
            
             if(windows_env) {
               stat_num <- lapply(unique(df_num[[group_vars]]), function(level) {
                 
                 tmp <- filter(df_num, across(all_of(group_vars), ~.x==level))
                 stats <- dplyr::select(index(), all_of(tmp$Index)) %>% 
                   tidyr::pivot_longer(everything(), names_to = "index", values_to = "value") %>% 
                   summarise(across(matches("value"), l_fun, na.rm = T, .names = "{fn}")) %>%
                   mutate(group = group_vars, level = level)
                 
                 stats$n <- as.integer(nrow(tmp))
                 
                 return(stats)
               }) # group level mean sd min max   
             } else {
               stat_num <- parallel::mclapply(unique(df_num[[group_vars]]), function(level) {
                 
                 tmp <- filter(df_num, across(all_of(group_vars), ~.x==level))
                 stats <- dplyr::select(index(), all_of(tmp$Index)) %>% 
                   tidyr::pivot_longer(everything(), names_to = "index", values_to = "value") %>% 
                   summarise(across(matches("value"), l_fun, na.rm = T, .names = "{fn}")) %>%
                   mutate(group = group_vars, level = level)
                 
                 stats$n <- as.integer(nrow(tmp))
                 
                 return(stats)
               }, mc.cores = max(4, parallel::detectCores()-4)) # group level mean sd min max
             }
            
          } # if group_vars exists
          
          stat_num <- stat_num %>% purrr::reduce(full_join) %>% distinct()
          if("n" %in% names(stat_num)) stat_num$prop = stat_num$n/sum(stat_num$n)
          val_report[[paste0(report_prefix, "stat_num")]] <- stat_num
          
          # output table
          renderRctTableModuleServer("stat_num", reactive(stat_num), T,
                                    # c("FixedHeader", "FixedColumns"),
                                    digits = reactive(input$view_dec), 
                                    downloadName = "sum_stat_index", #editable = F, 
                                    colfilter = F) #"none")
          
          # make df for histogram
# cat("  group_vars:", !is.null(group_vars), "df_num:\n");print(head(df_num))
          if(group_vars=="") { # plot 100 indexes
            
            df <- index() %>% 
              tidyr::pivot_longer(everything(), "index", values_to = "value")
            
            if(ncol(index()) > 100) { # down sample
              df <- slice_sample(df, n = 100, replace = F)
            }

            output$boxplot <- renderPlot({
              withProgress(
                message = "Plotting. This may take some time...",
                {
              req(df, input$font_size)
              width  <- session$clientData[[paste0("output_", session$ns("boxplot"), "_width")]]
              
              tmp <- arrange(stat_num, desc(mean)) %>% 
                filter(variable %in% df$index)
              
              p <- plotBox(input, output, session,
                          df, "index", "value", order = tmp$variable, # order doesn't work !!!
                          font_size = reactive(input$font_size))
              val_report[[paste0(report_prefix, "p")]] <- p
              
              downloadPlotModuleServer("dnld_boxplot", paste0("boxplot_", session$ns("name")),
                                       p, reactive(width))
              return(p)
            }) })
            # cat("  df 2:\n");print(head(df))
          } else {  # plot by group
            
            if(windows_env) {
              df <- lapply(unique(df_num[[group_vars]]), function(level) {
                
                tmp <- filter(df_num, across(all_of(group_vars), ~.x==level))
                return(dplyr::select(index(), all_of(tmp$Index)) %>% 
                         tidyr::pivot_longer(everything(), names_to = "index", values_to = "value") %>% 
                         mutate(level = level))
              })
            } else {
              df <- parallel::mclapply(unique(df_num[[group_vars]]), function(level) {
                
                tmp <- filter(df_num, across(all_of(group_vars), ~.x==level))
                return(dplyr::select(index(), all_of(tmp$Index)) %>% 
                         tidyr::pivot_longer(everything(), names_to = "index", values_to = "value") %>% 
                         mutate(level = level))
              }, mc.cores = max(4, parallel::detectCores()-4))
            }
            df <- do.call(rbind, df)
            
            output$boxplot <- renderPlot({
              withProgress(
                message = "Plotting. This may take some time...", value = 1,
                {
              req(df, input$font_size)
              width  <- session$clientData[[paste0("output_", session$ns("boxplot"), "_width")]]
              
              p <- plotBox(input, output, session,
                           df, "level", "value", group = "level", xlab = group_vars,
                           font_size = reactive(input$font_size))
              val_report[[paste0(report_prefix, "p")]] <- p
              
              downloadPlotModuleServer("dnld_boxplot", paste0("boxplot_", session$ns("name")),
                                       p, reactive(width))
              return(p)
            }) })
          } # if group_vars==""
        } # if index_vars exists
        
      }, ignoreInit = T) # observeEvent
    })}