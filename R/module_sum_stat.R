sumstatModSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # selectInput(ns("dat_sel"), "Datasets:", c("None", "gizmo", "score", "ranking", "combined"),
                # "None"),
    uiOutput(ns("ui_vars")),
    uiOutput(ns("ui_group")),
    selectInput(ns("functions"), "Apply function(s):",
                c("mean", "sd", "min", "max", "n_missing", "n_obs","prop"),
                c("mean", "sd", "min", "max", "n_obs","n_missing"),
                multiple = T),
    numericInput(ns("view_dec"), "Decimals:", value = 2, min = 0),
    numericInput(ns("font_size"), "Font size", 12, 1, 20, 1),
    br(),
    actionButton(ns("create_table"), "Create table", icon("table"), class = "btn-success")
  )
}

sumstatModUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("warning1")),
    textOutput(ns("warning2")),
    h2("Numerical variable summary statistics"),
    h3("Table"),
    renderDtTableModuleUI(ns("stat_num"), "Numeric sum stats"),
    br(),br(),
    h3("Distribution"),
    plotOutput(ns("hist_num"), height = "800px"), # default 400px
    downloadModuleUI(ns("dnld_hist_num")),
    br(),br(),
    h2("String variable summary statistics"),
    h3("Table"),
    renderDtTableModuleUI(ns("stat_chr"), "String sum stats"),
    br(),br(),
    h3("Distribution"),
    plotOutput(ns("bar_char")),
    downloadModuleUI(ns("dnld_bar_char"))
    #br(),br(),
    # h2("Mult-choice variable frequency table"),
    # renderDtTableModuleUI(ns("stat_lst"), "N choice stats")
    # level conversion table for rating vars
  )
}

# val$data_filtered, val$onekmind_cleaned, val$rank, val$merged_filtered
sumstatMod <- function(id, dat = reactive(NULL), #val = reactive(NULL),
                       ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # initialize
      tempVar <- reactiveValues()
      
      # warning messages
      output$warning1 <- renderText({
# cat("sumstatUnivarMod"); print()
        validate(
          need(length(dat()) > 0,
               paste0("The data",
                      " does not exist, please upload one or click 'Rename and save' on filter tab")
          ))
      })

      # select dataset from dropdown list, update variables
      observeEvent(length(dat()) > 0, {
# cat("sumstatMod\n observe dat:");print(dim(dat())); cat("\n")
        req(!is.null(dat()) && is.null(input$warning1))
# cat("  req satisfied\n")
        tempVar$dat <- dat()
# cat("val name ", get(paste0(input$dat_sel, "_dat_name"))(), "\n")
# print(dim(tempVar$dat))
        tempVar$vars <- names(tempVar$dat)
        tempVar$vars_n_types <-
          rlang::set_names(paste0(tempVar$vars, "{", sapply(tempVar$dat, typeof), "}"))
        # print("sumstatUnivarMod"); print(head(tempVar$vars_n_types))
      })
      
      # col var selection panel at sidebar
      output$ui_vars <- renderUI({
        req(tempVar$vars_n_types)
        
        # vars <- grep("numeric|integer|double", tempVar$vars_n_types, value = T)
        return(
          selectInput(
            session$ns("vars"),  # use session$ns() to get inside id being recognized
            "Variable(s):", choices = tempVar$vars_n_types,
            selected = "", # state_multiple("view_vars", vars, vars),
            multiple = T, selectize = FALSE, size = 8
          ))
      }) # ui_vars
      
      # Group by
      output$ui_group <- renderUI({
        req(tempVar$vars_n_types!="")
        
        yvars <- grep("character|factor|logical|integer", tempVar$vars_n_types, value = T) %>%
          base::setdiff(input$vars)
        
        return(
          selectInput(session$ns("group_vars"), "Group by:", choices = c("", yvars),
                      selected = ""))
      })
      
      # transpose
      
      # warning messages
      output$warning2 <- renderText({
        req(input$create_table > 0)
        
        validate(
          need(!is.null(tempVar$dat), "Please select a clean dataset"),
          need(input$vars!="", "Please select a X variable")
        )
      })
      
      # sumstat table
      observeEvent(input$create_table, {
        req(!is.null(tempVar$dat), input$vars!="")
        # print("observe create table")
        # extract input elements
        varss <- isolate(sapply(strsplit(input$vars, "\\{"), head, 1) %>% unlist()) # remove {type}
        var_type <- sapply(tempVar$dat[,varss, drop = F], class) %>% sapply(head, 1)
        
        group_vars <- isolate(sapply(strsplit(input$group_vars, "\\{"), head, 1) %>% unlist())
        if(length(group_vars)==0) {
          group_vars <- NULL # convenient for dplyr functions
        }
        
        l_fun <- list("mean" = mean, "sd" = sd, "min" = min, "max" = max) # add n_obs, n_missing later
        l_fun <- l_fun[which(names(l_fun) %in% input$functions)]
        
        vars_num <- varss[grepl("numeric|integer|double", var_type)]
        vars_chr <- varss[grepl("character|factor|logical|POSIXct|POSIXt", var_type)]
        # vars_lst <- varss[grepl("list", var_type)]
        # 
        # if(length(vars_lst) > 0) { # unlist list cols
        #   
        #   if(length(vars_lst) > 1) {
        #     lst_type <- select_at(tempVar$dat, vars(all_of(vars_lst))) %>%
        #       sapply(unlist) %>% sapply(class) %>% sapply(head, 1)
        #   } else {
        #     lst_type <- select_at(tempVar$dat, vars(all_of(vars_lst))) %>%
        #       unlist() %>% class() %>% head(1)
        #   }
        #   
        #   # cat("lst_type"); print(lst_type)
        #   vars_num <- c(vars_num, vars_lst[grepl("numeric|integer|double", lst_type)])
        #   vars_chr <- c(vars_chr,
        #                 vars_lst[grepl("character|factor|logical|POSIXct|POSIXt", lst_type)])
        #   #cat("vars_lst grepl"); print(vars_lst[grepl("character|factor|logical|POSIXct|POSIXt", lst_type)])
        #   ## make a special table for list vars
        #   stat_lst <- do.call(rbind, lapply(vars_lst, function(var_lst) {
        #     # cat(" lapply var_lst ", var_lst, "\n")
        #     n_choice <- sapply(tempVar$dat[,var_lst,drop = T], length)
        #     #cat("\n  n_choice 1"); print(head(n_choice))
        #     n_choice[which(is.na(tempVar$dat[,var_lst, drop = T]))] <- NA
        #     #cat("\n  n_choice 2"); print(head(n_choice))
        #     #print(data.frame(table(n_choice, useNA = "always")))
        #     # variable n_choice Freq
        #     stat_nchoice <- data.frame(variable = var_lst, table(n_choice, useNA = "always"))
        #     #cat("\n  stat_nchoice"); print(head(stat_nchoice))
        #   }))
        #   # cat("stat_lst\n"); print(head(stat_lst))
        #   renderDtTableModuleServer("stat_lst", reactive(stat_lst), F,
        #                             c("FixedHeader", "FixedColumns"),
        #                             digits = reactive(0), downloadName = "sum_stat_nchoice",
        #                             editable = F, colfilter = "none")
        # } # if vars_lst exists
        # cat("varss", paste(varss, collapse = " "),
        #     ", var_type", paste(var_type, collapse = ""),
        #     ", group_vars", paste(group_vars, collapse = ""), "\n")
        # cat("vars_lst", paste(vars_lst, collapse = ""), "\n",
        #     "vars_num", paste(vars_num, collapse = ""), "\n",
        #     "vars_chr", paste(vars_chr, collapse = ""), "\n")
        
        ## sumstat numeric variables
        if(length(vars_num) > 0) {
cat("vars_num exists --> stat_num lapply\n")
          df_num <- dplyr::select(tempVar$dat, all_of(c(group_vars, vars_num)))

          stat_num <- lapply(vars_num, function(var_num) {
            # cat(" var_num", var_num, "\n")
            if(class(df_num[[var_num]]) == "list") {
              df <- tidyr::unnest_longer(df_num, var_num)
            } else {
              df <- df_num
            }
# cat(" df"); print(head(df))
            stats <- df %>% dplyr::select(all_of(c(group_vars, var_num))) %>%
              group_by_at(vars(all_of(group_vars))) %>% # group_by(across(all_of(group_vars))) %>% # doesn't work with group_vars==NULL
              summarise(across(all_of(var_num), l_fun, na.rm = T, .names = "{fn}")) %>% # summarise_at(var_num, l_fun, na.rm = T) %>%
              mutate(variable = var_num)
# cat(" stats1:");print(head(stats))
            if(ncol(stats) < length(group_vars) + length(l_fun) + 1) { # sanity check
              return(NULL)
            } else {
              stats <- dplyr::select(stats, all_of(c(group_vars, "variable", names(l_fun))))
            }
# cat(" stats2:");print(head(stats))
            if("n_missing" %in% input$functions) {
              miss <- df %>% select_at(vars(all_of(group_vars), all_of(var_num))) %>%
                group_by_at(vars(all_of(group_vars), all_of(var_num))) %>%
                tally() %>%
                filter_at(vars(all_of(var_num)), any_vars(is.na(.))) %>%
                select_at(vars(-all_of(var_num)))

              if(nrow(miss) == 0) {
                miss <- df %>% select_at(vars(all_of(group_vars))) %>%
                  group_by_at(vars(all_of(group_vars))) %>%
                  mutate(variable = var_num, n_missing = 0)
# cat("nrow miss == 0"); print(dim(miss))
              } else {

                miss <- miss %>% mutate(variable = var_num)
                names(miss)[ncol(miss)-1] <- "n_missing"
              }
              
              stats <- left_join(stats, miss, by = c(all_of(group_vars), "variable")) %>%
                mutate(n_missing = as.character(n_missing))
              stats$n_missing[which(is.na(stats$n_missing))] <- "0" # 14june2021
            } # n_missing
            
            return(stats)
          }) # lapply
          
          # variable   mean    sd   min   max n_missing
          stat_num <- stat_num %>% purrr::reduce(full_join) %>% distinct()
          if("n_obs" %in% input$functions) {
            if(!is.null(group_vars)) {
              ns <- df_num %>% group_by_at(vars(all_of(group_vars))) %>%
                summarise(n = n())
              
            } else {
              ns <- data.frame(n = nrow(df_num))
            }
 
            if("prop" %in% input$functions) { # not by group
              ns <- ns %>% mutate(prop = n/sum(n))
            }
            
            if(length(vars_num) == 1) { # input$vars is one numeric var
              ns$variable <- vars_num
              
            } else {
              ns <- do.call(rbind, lapply(vars_num, function(var_num) {
                out <- ns
                out$variable <- var_num
                return(out)
              }))
            }
# cat(" stat_num:");print(head(stat_num)); print(head(ns))
            
            # variable   mean    sd   min   max n_missing n prop
            stat_num <- full_join(stat_num, ns) %>% distinct()
            
            if("n_missing" %in% names(stat_num)) { # 14/6/2021
              stat_num <- mutate(stat_num, n_missing = as.integer(n_missing),
                                 n_obs = as.character(n - n_missing)) %>% 
                mutate(n_missing = as.integer(n_missing)) # %>%
            # filter(n != n_missing)
            }
            stat_num  <- mutate(stat_num, n = as.character(n))
          } # if n_obs in input$functions
          
          # output table
          renderDtTableModuleServer("stat_num", reactive(stat_num), T,
                                    c("FixedHeader", "FixedColumns"),
                                    digits = reactive(input$view_dec), downloadName = "sum_stat_num",
                                    editable = F, colfilter = "none")
          
          # make df for histogram
# cat("  group_vars:", !is.null(group_vars), "df_num:\n");print(head(df_num))
          if(!is.null(group_vars)) {
            df <- tidyr::pivot_longer(df_num, all_of(group_vars),
                                    names_to = "group", values_to = group_vars) %>%
# cat("  df 1:\n");print(head(df))
              dplyr::select(-group)
# cat("  df 2:\n");print(head(df))
          } else {
            df <- df_num
          }
          
          # group var value ...  
          df <- df %>% tidyr::pivot_longer(all_of(vars_num), "var", values_to = "value")
# cat("  df 3:\n");print(head(df));#write.table(df, "../test outputs/half_indexes/df", quote = F, row.names = F)
          output$hist_num <- renderPlot({
            req(df)
# cat(" renderPlot\n")
            group2 <- NULL # ifelse can't return NULL without error
            if(!is.null(group_vars)) group2 <- "var"
            width  <- session$clientData[[paste0("output_", session$ns("hist_num"),
                                                 "_width")]]
            p <- plotHist(input, output, session,
                          df, "value",
                          group1 = ifelse(!is.null(group_vars), group_vars, "var"),
                          group2 = group2, nbins = 30,
                          font_size = reactive(input$font_size))

            downloadPlotModuleServer("dnld_hist_num",
                                     paste0("histogram_", "_numeric", session$ns("name")),
                                     p, reactive(width))
            return(p)
          })
        } # if vars_num exists
        
        ## sumstat character|factor|logical|integer variables
        if(length(vars_chr) > 0) {
          # cat("vars_chr exists --> stat_chr lapply\n")
          df_chr <- select_at(tempVar$dat, vars(all_of(group_vars), all_of(vars_chr)))
          
          stat_chr <- lapply(vars_chr, function(var_chr) {
            # cat(" var_chr", var_chr, "\n")
            if(class(df_chr[[var_chr]]) == "list") {
              df <- tidyr::unnest_longer(df_chr, var_chr)
              # cat("  unnest_longer\n"); print(head(df))
            } else {
              df <- df_chr
            }
            
            stats <- df %>% select_at(vars(all_of(group_vars), all_of(vars_chr))) %>%
              group_by_at(vars(all_of(group_vars), all_of(vars_chr))) %>%
              tally() %>%
              mutate(variable = var_chr)
# cat("sumStatMod\n   1 stats\n"); print(head(stats))
            stats <- select_at(stats, vars(all_of(group_vars), "variable", all_of(var_chr), "n"))
            
            if("prop" %in% input$functions) { # not by group
              stats <- stats %>% mutate(prop = n/sum(n))
            }
            
            if("n_missing" %in% input$functions) {       
              stats$n_missing <- 0
              idx <- which(is.na(stats[[var_chr]]))
              stats$n_missing[idx] <- stats$n[idx]
              # cat("   if n_missing\n"); print(head(stats))
              stats$n_obs <- stats$n - stats$n_missing # 14june2021
              stats$n_missing <- as.character(stats$n_missing)
              stats$n <- as.character(stats$n)
              stats$n_obs <- as.character(stats$n_obs)
            }
            
            names(stats)[which(names(stats)==var_chr)] <- "level"
            
            return(stats)
          })

          # group_var variable level n prop n_missing
          stat_chr <- stat_chr %>% purrr::reduce(full_join) %>% distinct()
# cat("  2 stat_chr\n"); print(head(stat_chr))
          renderDtTableModuleServer("stat_chr", reactive(stat_chr), T,
                                    c("FixedHeader", "FixedColumns"),
                                    digits = reactive(input$view_dec),
                                    downloadName = "sum_stat_chr", editable = F, colfilter = "none")
        } # if vars_char exist
        
        #         df1 <- do.call(cbind, lapply(1:ncol(df), function( i ) {
        # print(i)
        #           if(class(df[[i]])=="character") {
        #             return(as.numeric(as.factor(df[[i]])))
        #
        #           } else if(class(df[[i]])=="factor") {
        #               return(as.numeric(df[[i]]))
        #           } else {
        #               return(df[[i]])
        #           }
        #         }))
        
        # for rating vars as character strings, do as.numeric(as.factor(v)) for sum stat
        # --> show a level conversion table at the bottom
        
        
        #renderDtTableModuleServer("stat_chr", reactive(stat_chr), downloadName = "sum_stat_chr")
        #renderDtTableModuleServer("", reactive(), downloadName = "sum_stat_")
        
      }) # observe input$creat_table
    })}