library(markdown) # source here otherwise manifest doesn't see this dependency

# preload files
desc_ebv <- readxl::read_xlsx("data/description_bv.xlsx", col_names = T) 
# column_labelling, classifer; ID, ClassVar, EV, (Group, )
desc_ev <- read.csv2("data/description_ev.csv", sep = ",", 
                     col.names = c("column_labelling", "classifier"))
# ID, (sex, RM, ...), trait1, trait2, ... (trait1_ACC, trait2_ACC...)
dat_ebv <- read.table("data/bv.csv", 
                      colClasses = c(rep("character", 2), rep("double", 14)),
                      header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                      quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                      strip.white = T)
# ID, (line, group1), ... trait1, trait2, ...
dat_ev <- read.table("data/ev.csv", 
                     colClasses = c("character", rep("double", 11), rep("character", 2)),
                     header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                     quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                     strip.white = T)
# Index weight (weight2) ...
dat_w <- read.table("data/index_weight.csv",
                    colClasses = c("character", "double"),
                    header = T, sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = F,
                    quote = "\"", fill = T, comment.char = "", dec=".", check.names = F,
                    strip.white = T)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## OPTIONS ###
  # allow file sizes up to 300MB
  options(shiny.maxRequestSize = 300 * 1024 ^ 2, shiny.trace = F
          , shiny.error = NULL, # browser 
          rsconnect.max.bundle.size = 8192 * 1024^2 # largest memory size available
          
  )
  # print(Sys.getenv())
  cat("N cores:", parallel::detectCores(), "\n") # rsconnect.nonprod... 8 cores/processor
  print(dir())
  ## INITIALIZE, load demo ##
  shinyjs::hide("initial_warn", T, "fade", 10)
  
  val <- reactiveValues()
  val_report <- reactiveValues()
  
  observeEvent(input$demo, {
    # column_labelling, classifier; ID, ClassVar, EBV, (Group, Order, Unit)
    val$desc_ebv <- desc_ebv
    # column_labelling, classifer; ID, ClassVar, EV, (Group, )
    val$desc_ev <- desc_ev
    # ID, (sex, RM, ...), trait1, trait2, ... (trait1_ACC, trait2_ACC...)
    val$dat_ebv <- dat_ebv
    # ID, (line, group1), ... trait1, trait2, ...
    val$dat_ev <- dat_ev
    # Index weight (weight2) ...
    val$dat_w <- dat_w
    # cat("observe input$demo val"); print(sapply(reactiveValuesToList(isolate(val)), head)    )
  })
  
  ### SHOW/HIDE HELP .Rmd ###
  observeEvent(input$help_btn_ebv_filter, {
    if(input$help_btn_ebv_filter %% 2 == 1){
      shinyjs::show("help_html_ebv_filter")
    }else{
      shinyjs::hide("help_html_ebv_filter")
    }
  })
  
  observeEvent(input$help_btn_ev_filter, {
    if(input$help_btn_ev_filter %% 2 == 1){
      shinyjs::show("help_html_ev_filter")
    }else{
      shinyjs::hide("help_html_ev_filter")
    }
  })
  
  ## UPLOAD DATA ##
  # return val$desc_ebv, val$desc_ev, val$dat_ebv, val$dat_ev and/or val$dat_w
  preprocessUploadMod("step1", val) # reactive(val))
  
  ## CLEAN DATA ##
  # change format etc
  observeEvent(input$upload == 'tab.step2' || input$upload == 'tab.step3', {
    # cat("observe tab.step2\n"); print(names(val));print(length(val));# val is not a list
    # print(length(reactiveValuesToList(val))); print(sapply(reactiveValuesToList(isolate(val)), length))
    req(length(reactiveValuesToList(val)) <= 5 && length(reactiveValuesToList(val)) >= 4) # if uploaded new files after calculations, won't react
    req(length(val$desc_ebv) > 0 && length(val$desc_ev) > 0 && 
          length(val$dat_ebv) > 0 && length(val$dat_ev) > 0)
    # cat(" req2 satisfied\n")
    val$dt_description_clean <- cleanDescData(val$desc_ebv)
    val$dt_ebv_clean <- cleanEbvData(val$dt_description_clean, val$dat_ebv)
    val$dt_desc_ev_clean <- val$desc_ev
    val$dt_ev_clean <- cleanEVplant(val$dt_desc_ev_clean, val$dat_ev)
    
    if("dat_w" %in% names(val)) {
      val$dt_w_clean <- cleanW(val$dat_w) 
    }
  })
  
  ## FILTER EBV ##
  # NA filter is on the UI (ebv_na, acc_na)
  
  # character/factor filter
  stefanFilterMod("stfn_ebv", dt = reactive(val$dt_ebv_clean))
  
  filter_levels <- eventReactive(input$`stfn_ebv-apply`, {
    # print("event reactive stefan button")
    req(val$dt_ebv_clean)
    # cat("event reactive stefan button\n")
    req(!is.null(input$`stfn_ebv-filter_col`))
    
    filter_cols <- input$`stfn_ebv-filter_col`
    
    filter_levels <- lapply(seq(filter_cols), function(i) {
      return(input[[paste0("stfn_ebv-", i)]])
    })
    names(filter_levels) <- seq(filter_cols)
    
    if(any(sapply(filter_levels, length)==0)) { # sanity check
      output$stefan_filter_error_message <- renderUI({
        renderText("Please select filter levels to apply")
      })
    }
    return(filter_levels)
  })
  
  # show and download data table. Apply column filters and the search bar.
  # the updated datatable is stored in val$data_filtered
  dataViewerModuleServer("ebv_filter", reactive(val$dt_ebv_clean), val,
                         filter_dat_name = "dt_ebv_filtered",
                         filter_cols = reactive(input$`stfn_ebv-filter_col`),
                         filter_levels = filter_levels,
                         na_include = reactive(input$ebv_na_0), na_to_0 = reactive(input$ebv_na_0),
                         apply = reactive(input$`stfn_ebv-apply`)
  )
  
  observeEvent(input$ebv_na_0, {
    if(input$ebv_na_0) {
      shinyjs::show("warn_ebv_0")
    } else {
      shinyjs::hide("warn_ebv_0")
    }
  })
  
  ## SUMMARY STATISTICS EBV ##
  sumstatMod("sumstat_ebv", reactive(val$dt_ebv_filtered), NULL, val_report,
             report_prefix = "sumstat_ebv-")
  
  ## FILTER EV ##
  # NA filter is on the UI (ebv_na, acc_na)
  
  # character/factor filter
  stefanFilterMod("stfn_ev", dt = reactive(val$dt_ev_clean))
  
  filter_levels_ev <- eventReactive(input$`stfn_ev-apply`, {
    # print("event reactive stefan button")
    req(val$dt_ev_clean)
    # cat("event reactive stefan button\n")
    req(!is.null(input$`stfn_ev-filter_col`))
    
    filter_cols <- input$`stfn_ev-filter_col`
    
    filter_levels <- lapply(seq(filter_cols), function(i) {
      return(input[[paste0("stfn_ev-", i)]])
    })
    names(filter_levels) <- seq(filter_cols)
    
    if(any(sapply(filter_levels, length)==0)) { # sanity check
      output$stefan_filter_error_message_ev <- renderUI({
        renderText("Please select filter levels to apply")
      })
    }
    return(filter_levels)
  })
  
  # show and download data table. Apply column filters and the search bar.
  # the updated datatable is stored in val$dt_ev_filtered & val$dt_ebv_filtered
  dataViewerModuleServer("ev_filter", reactive(val$dt_ev_clean), val,
                         filter_dat_name = "dt_ev_filtered",
                         filter_cols = reactive(input$`stfn_ev-filter_col`),
                         filter_levels = filter_levels_ev,
                         na_include = reactive(input$ev_na_0), na_to_0 = reactive(input$ev_na_0),
                         apply = reactive(input$`stfn_ev-apply`)
  )
  
  ## SUMMARY STATISTICS EBV ##
  sumstatMod("sumstat_ev", reactive(val$dt_ev_filtered), xlab = "Economic Value ($)", 
             val_report, report_prefix = "sumstat_ev-")
  
  ## CALCULATE INDEX ##
  
  # Create val$dt_index
  observeEvent(input$plant_app, { # react when change to other tabs as well
    # observeEvent( length(val$dt_ev_filtered)>0 && length(val$dt_ebv_filtered) > 0, {
    # cat("observe plant_app\n len val:", length(reactiveValuesToList(val)),"\n");#print(names(val))
    # cat(" plant_app:", input$plant_app, "\n")
    req(input$plant_app == 'tab.index' && input$view_index == "tab.index1")
    req(length(reactiveValuesToList(val)) <= 12 && 
          length(reactiveValuesToList(val)) >=10) # avoid re-calculate when downstream analysis is already triggered
    req(val$dt_ev_filtered, val$dt_ebv_filtered, val$dt_description_clean, val$dt_desc_ev_clean)
    # cat(" reqs satisfied\n")
    # cat(" dt_ev_filtered:");print(val$dt_ev_filtered$Index)
    output$index_view_warn <- renderText({"Creating index, please wait..."})
    
    # ID, sex, ..., trait1, trait2, ... index1, index2, ...
    val$dt_sub_ebv_index_ids <- calculateIndividualBW(input, output, session,
                                                      val$dt_ebv_filtered, val$dt_ev_filtered, val$dt_description_clean,
                                                      val$dt_desc_ev_clean)
    
    # ID, sex, ... index1, index2...
    val$dt_sub_index_ids <- 
      val$dt_sub_ebv_index_ids[,!names(val$dt_sub_ebv_index_ids)
                               %in% val$dt_description_clean$column_labelling[
                                 val$dt_description_clean$classifier=="EBV"] ]
    # cat(" dt_sub_index_ids:");print(tail(colnames(val$dt_sub_index_ids), -2))    
    # animal ID x Index
    val$dt_index <- dplyr::select(val$dt_sub_index_ids, matches(val$dt_ev_filtered$Index))
    # %>% t() %>% data.frame()
    rownames(val$dt_index) <- val$dt_sub_index_ids$ID # rownames may not auto get from original colnames
    # print(match(names(val$dt_sub_index_ids), val$dt_ev_filtered$Index))
    # cat("observe plant_app, dim val$dt_index:");print(dim(val$dt_index));print(val$dt_index[1:3,1:3])
    output$index_view_warn <- renderText({
      "Index calculated. Now you can download and go to the next steps, or wait for the summary
      statistic table and boxplot to load."})
  })
  
  ## INDEX STATISTICS ##
  observeEvent(val$dt_index, { #"dt_index" %in% names(val), { # only observe once
    # cat("observe dt_index, val: ");print(names(val))
    req(!is.null(val$dt_index))
    
    output$index_view_n <- renderPrint({
      paste0("You have ", nrow(val$dt_index), " individuals and ", ncol(val$dt_index), " indexes. ",
             "Individuals with missing EBVs are removed.")
    })
  })
  indexSumstatMod("index_view", reactive(val$dt_index), val, val_report, "index_view-")
  
  ## Find CLUSTER ##
  
  # # a smaller data
  # votes.repub <- cluster::votes.repub[
  #   which(apply(cluster::votes.repub, 1, is.na) %>% apply(2, sum) ==0),] # states by features
  # 
  # observeEvent(votes.repub,{
  #   req(length(votes.repub) > 0)
  #   val$dt_index <- data.frame(t(votes.repub)) # feature x states
  #   print(dim(val$dt_index))
  #   })
  # 
  # cl <- clusteringMod("find_cl", val, dat = reactive(val$dt_index), transpose = F)
  
  # return list(cluster_obj, clusters). Create val$cl.
  # if didn't run finalCluster will return list(cluster_obj, best_method, agg_coefs)
  # with the simulation it takes 6 min to find an agglomerative method, 6.5 min to run wss for k,
  # and 6 min to run silhouette for k
  cl <- clusteringMod("find_cl", val,
                      dat = reactive(val$dt_index), # col_sel = reactive(val$dt_ev_filtered$Index),
                      transpose = F,
                      val_report, "find_cl-")
  
  ## CLUSTER SUMMARY STATISTICS ##
  
  # need val$dt_index, val$cl
  clusterSumStatMod("cl_sumstat", val, cl, transpose = F, 
                    val_report = val_report, report_prefix = "cl_sumstat-")
  
  ## CLUSTER DIAGNOSIS ##
  
  # need val$dt_index, val$cl. if upload new files, fill val$cl with list(cluster_obj, clusters)
  clusterDxMod("Dx", val, 
               transpose = T, reactive(input$`find_cl-center`), reactive(input$`find_cl-scale`),
               val_report = val_report, report_prefix = "Dx-")
  
  ## AGGREGATION ##
  
  # CREATE INDEX WEIGHT GIVEN CLUSTERING RESULTS #
  # need val$dt_index,  val$cl, val$dt_ev_filtered, val$dt_desc_ev_clean (for user group)
  # create val$dt_weight (data.frame), a data.frame of 3 columns: Index, cluster and weight;
  # val$dt_ev_agg, a data.frame of columns as Index, cluster and traits (EVs); and
  # val$dt_ev_avg, same structure as above
  calWeiMod("cl_weight", val, transpose = F, # 15june2021 this occurred twice. 2nd time new_index_1 are NAs
            val_report = val_report, report_prefix = "cl_weight-")
  
  # AGGREGATED INDEX DIAGNOSIS #
  
  # look at correlations among new and old indexes, and top individual overlap among them
  # need val$dt_ev_agg, val$dt_ebv_filtered, val$dt_description_clean
  # create val$dt_index_new. The same as dt_index but with new_index in it.
  dt_index_sub <- aggDxMod("agg_dx", val, transpose = F, reactive(val$cl$clusters), 
                           reactive(val$dt_ev_agg), reactive(val$dt_index),
                           val_report, "agg_dx-")
  
  # top individual overlap/agreement
  aggDxMod2("agg_dx2", transpose = F, 
            dt_index_sub, reactive(val$dt_index),
            reactive(input$`agg_dx-sel_index`), reactive(input$`agg_dx-sel_agg`),
            reactive(input$`agg_dx-sel_cluster`),
            val_report, "agg_dx2-"
  )
  
  # look at classVar pattern among aggregated indexes and user-defined groups
  # need val$dt_index_new and others in val.
  aggDxMod3("agg_dx3", val, transpose = F, reactive(val$cl$clusters),
            val_report, "agg_dx3-")
  
  # look at EV weighting pattern among aggregated indexes and user-defined groups
  # need val$dt_w_clean
  # need val$dt_ev_agg, a data.frame of columns as Index, cluster and traits (EVs); and
  # val$dt_ev_avg, same structure as above
  aggDxMod4("agg_dx4", val, 
            reactive(val$dt_ev_agg), reactive(val$dt_w_clean), reactive(val$cl$clusters),
            val_report, "agg_dx4-")
  
  # REPORT #
  # modulising this makes all parameters NULL reportMod("report")
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("report_index_aggregation_", # "report_index_aggregation.doc",
                      Sys.Date(), ".",
                      switch(input$report_format,
                             Word = "docx", HTML = "html", PDF = "pdf")),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      temp_dir <- tempdir() # knit from here. Need all input in there
      temp_report <- file.path(temp_dir, "report.Rmd")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      
      temp_template <- file.path(temp_dir, "template_ab.docx")
      file.copy("report_template/template_ab.docx", temp_template, overwrite = T)
      # file.copy("report_template/template_default_tableStyle.docx", temp_report, overwrite = T)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        demo = input$demo,
        file_name_ebv = input$`step1-dat_ebv-file`$name,
        file_name_ev = input$`step1-dat_ev-file`$name,
        file_name_wt = input$`step1-dat_wt-file`$name,
        
        filter_col_ebv = input$`stfn_ebv-filter_col`,
        filter_level_ebv = filter_levels,
        filter_col_ev = input$`stfn_ev-filter_col`,
        filter_level_ev = filter_levels_ev,
        filter_ebv_na_0 = input$ebv_na_0,
        
        sumstat_ebv_table = val_report$`sumstat_ebv-stat_num_table`,
        sumstat_ebv_digit = input$`sumstat_ebv-view_dec`,
        sumstat_ebv_p = val_report$`sumstat_ebv-sumstat_num_p`,
        sumstat_ebv_table_str= val_report$`sumstat_ebv-sumstat_str_table` ,
        sumstat_ebv_p_str = val_report$`sumstat_ebv-sumstat_str_p`,
        
        sumstat_ev_table = val_report$`sumstat_ev-stat_num_table`,
        sumstat_ev_digit = input$`sumstat_ev-view_dec`,
        sumstat_ev_p = val_report$`sumstat_ev-sumstat_num_p`,
        sumstat_ev_table_str= val_report$`sumstat_ev-sumstat_str_table` ,
        sumstat_ev_p_str = val_report$`sumstat_ev-sumstat_str_p`,
        
        n_indi = nrow(val$dt_index),
        n_index = ncol(val$dt_index),
        sumstat_index = val_report$`index_view-stat_num`,
        sumstat_index_digit = input$`index_view-view_dec`,
        sumstat_index_p = val_report$`index_view-p`, # 100 samples
        
        cl_input = input$`find_cl-which_data`,
        cl_k = input$`find_cl-k_slider`,
        cl_agg = input$`find_cl-agg_method`,
        cl_best_method = val$cl$best_method,
        cl_agg_coefs = val$cl$agg_coefs,
        cl_op_cut = val_report$`find_cl-op_cut`, # rctv? $p_tss, p$sil
        
        cl_cor = val_report$`cl_sumstat-cor_table`,
        cl_cor_digit = input$`cl_sumstat-view_dec`,
        # slow to generate a histogram of large inputs in the app already
        # when generating a report, repeat this process even more slowly
        cl_cor_p = val_report$`cl_sumstat-cor_p`,
        cl_show_cor = input$`Dx-show_corr`,
        cl_heat = val_report$`Dx-heatmap`,
        # cl_heat_data = val_report$`Dx-data`,
        
        cl_choose_wt = input$`cl_weight-choose_w`,
        new_index_wt = val$dt_weight,
        new_ew_digit = input$`cl_weight-view_dec`,
        new_ew = val_report$`cl_weight-ew_new`,
        new_ew_p = val_report$`cl_weight-p_ew`,
        new_relew = val_report$`cl_weight-relew`,
        new_relew_p = val_report$`cl_weight-p_relew`,
        
        dx_bench = input$`agg_dx-sel_benchmark`,
        dx_cor = val_report$`agg_dx-cor`,
        dx_cor_p = val_report$`agg_dx-cor_p`,
        dx_sel_agg = input$`agg_dx-sel_agg`,
        dx_cor_title = val_report$`agg_dx-corr_title`,
        dx_cor_selected = val_report$`agg_dx-cor_selected`,
        dx_cor_p_selected = val_report$`agg_dx-cor_p_selected`,
        
        dx2_top_n_title = val_report$`agg_dx2-top_n_title`,
        # dx2_percent = input$`agg_dx2-percent`,
        dx2_top_n_table = val_report$`agg_dx2-top_n_table`,
        dx2_top_n_p = val_report$`agg_dx2-top_n_p`,
        
        dx3_classvar = input$`agg_dx3-class_var`,
        dx3_agg_by = input$`agg_dx3-agg_by`,
        dx3_summary = val_report$`agg_dx3-summary`,
        dx3_summary_p = val_report$`agg_dx3-summary_p`,
        
        dx4_wtvar = input$`agg_dx4-weight_var`,
        dx4_agg_by = input$`agg_dx4-agg_by`,
        dx4_summary = val_report$`agg_dx4-summary`,
        dx4_summary_p = val_report$`agg_dx4-summary_p`
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        temp_report,
        switch(input$report_format,
               Word = rmarkdown::word_document(toc = TRUE,
                                              reference_docx = "template_ab.docx"),
               HTML = rmarkdown::html_document(toc = T, toc_depth = 3,
                                               toc_float = T) # ,
               # PDF = rmarkdown::pdf_document(toc = T) # https://community.rstudio.com/t/unable-to-knit-to-pdf-in-r-markdown/133451/4
               # Word = "word_document", HTML = "html_document", PDF = "pdf_document"
        ),
        file,
        params = params,
        envir = new.env(parent = globalenv()) # use with params
      )
    }
  ) # downloadHandler
} # server

