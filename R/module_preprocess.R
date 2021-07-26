preprocessUploadModsidebarUI <- function(id, title = "Step 1 file uploads") {
  ns <- NS(id)
  tagList(
    # actionButton(ns("help_btn"), "", icon("question"), class = "btn btn-outline-light"),
    column(12, div(actionButton(ns("help_btn"), "", icon("question"), 
                                class = "btn btn-outline-info"), style = "float:right")),
    h4(title),
    helpText("Upload description files before value files"),
    wellPanel(
     # class = "bg-light",
      uploadTableModuleUI(ns("desc_ebv"), "EBV description"),
      textOutput(ns("error_m_1")),
      uploadTableModuleUI(ns("dat_ebv"), "EBV"),
      textOutput(ns("error_m_3")),
      
      uploadTableModuleUI(ns("desc_ev"), "EV description"),
      textOutput(ns("error_m_2")),
      uploadTableModuleUI(ns("dat_ev"), "EV"),
      textOutput(ns("error_m_4")),
      uploadTableModuleUI(ns("dat_wt"), "EV weight (optional)"),
      textOutput(ns("error_m_5"))
    )
  )
}

preprocessUploadModUI <- function(id) {
  ns <- NS(id)
  tagList(
   shinyjs::hidden(div(id = ns("help_html"),
                      # htmltools::includeMarkdown("help/preprocess.Rmd")))
                     includeHTML(knitr::knit2html("help/preprocess.Rmd", fragment.only = TRUE)))),
   span(textOutput(ns("demo_message")), class = "text-info"),
   verbatimTextOutput(ns("sanity_message"))
  )
}

#'@param data_name A string. The name for reactive value \code{val} to save the outputed value
#'@param clean logic. If True then apply cleaning function. Default is \code{False}.
#'@param type A string. Options are "survey", "1000minds" or "rank". This determines the cleaning
#'       function to apply to the input data. Only use when \code{clean} == \code{True}.
#'@return val$desc_ebv, val$desc_ev, val$dat_ebv, val$dat_ev and/or val$dat_w
#'
preprocessUploadMod <- function(id, val, # data_name = "data", clean = T, type = "survey",
                                sheet = 1, skip = 0, desc_ebv = reactive(NULL),
                                ...) {
  moduleServer(
    id,
    function(input, output, session) {
cat("preprocessMod\n");
      flag <- F
      tempVar <- reactiveValues(
        cnvrt = data.frame(classifier = c("ID", "ClassVar", "Group", "EBV", "EV"),
                           colClasses = c(rep("character", 3), rep("numeric", 2))))
      
      observeEvent(input$help_btn, {
       if(input$help_btn %% 2 == 1){
          shinyjs::show("help_html")
       }else{
         shinyjs::hide("help_html")
       }
      })
      
      output$demo_message <- renderText({
       req("desc_ebv" %in% names(val))
          "You are using demo data now"
        })
      
      output$sanity_message <- renderText({"Choose demo or upload your own files"})
      
      ## EBV description file
      # user upload file
      desc_ebv <- uploadTableModuleServer("desc_ebv", sheet = sheet, skip = skip, 
                                          what = rep("character", 2))
      # what = classes())
      # show error message immediately below the uploading box
      output$error_m_1 <- renderText({
        validate(need(
          class(desc_ebv())!="try-error",
          attr(desc_ebv(), "condition")$message))
      })
      
      # detect if user uploaded a file
      observeEvent(length(desc_ebv()) > 0, { 
# cat(" observe desc_ebv "); cat(length(desc_ebv()), "\n")
        txt <- sanityCheckEBVdesc(desc_ebv())
        output$sanity_message <- renderText({ txt })
# cat(" txt:", txt, "\n")        
        if(is.null(txt)) {
# cat(" is.null txt")
          val$desc_ebv <- desc_ebv()
          output$demo_message <- renderText({ "You are using uploaded data now" })
          tempVar$ebv_colClasses <- dplyr::left_join(val$desc_ebv, tempVar$cnvrt, by = "classifier")
          
        } else {
          flag <- T
        #  shinyjs::delay(30000, stopApp()) # 30 seconds
        }
      })
      
      ## EV description file
      desc_ev <- uploadTableModuleServer("desc_ev", sheet = sheet, skip = skip,
                                         what = rep("character", 2))
      
      output$error_m_2 <- renderText({
        validate(need(
          class(desc_ev())!="try-error",
          attr(desc_ev(), "condition")$message))
      })
      
      observeEvent(length(desc_ev()) > 0, { 
        #cat("preprocessMod\n observe desc_ebv ");cat(length(desc_ebv()), "\n")
        txt <- sanityCheckEVdesc(desc_ev(), desc_ebv())
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$desc_ev <- desc_ev()
          tempVar$ev_colClasses <- dplyr::left_join(val$desc_ev, tempVar$cnvrt, by = "classifier")
          
        } else {
          flag <- T
       #   shinyjs::delay(30000, stopApp())
        }
      })
      
      ## EBV file
      dat_ebv <- uploadTableModuleServer("dat_ebv", sheet = sheet, skip = skip, 
                                         what = tempVar$ebv_colClasses$colClasses)
      
      output$error_m_3 <- renderText({
        validate(need(
          class(dat_ebv())!="try-error",
          attr(dat_ebv(), "condition")$message))
      })
      
      observeEvent(length(dat_ebv()) > 0, { 
# cat(" observe dat_ebv\n")
        output$sanity_message <- renderText({
          if(!"desc_ebv" %in% names(val)) {
            "Need to upload an EBV description file first. Please reload the app and try again."
          }
        })
        txt <- sanityCheckEBV(dat_ebv(), desc_ebv())
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$dat_ebv <- dat_ebv()
        } else {
          flag <- T
      #    shinyjs::delay(30000, stopApp())
        }
      })
      
      ## EV file
      dat_ev <- uploadTableModuleServer("dat_ev", sheet = sheet, skip = skip, 
                                        what = tempVar$ev_colClasses$colClasses)
      
      output$error_m_4 <- renderText({
        validate(need(
          class(dat_ev())!="try-error",
          attr(dat_ev(), "condition")$message))
      })
      
      observeEvent(length(dat_ev()) > 0, { 
# cat("preprocessMod\n observe desc_ebv ");cat(length(desc_ebv()), "\n")
        output$sanity_message <- renderText({
          if(!"desc_ev" %in% names(val))  {
            "Need to upload an EV description file first. Please reload the app and try again."
          }
        })
        txt <- sanityCheckEV(dat_ev(), desc_ev())
        output$sanity_message <- renderText({ txt })
        
        if(is.null(txt)) {
          val$dat_ev <- dat_ev()
        } else {
          flag <- T
      #    shinyjs::delay(30000, stopApp())
        }
      })
      
      ## EV weight file
      dat_w <- uploadTableModuleServer("dat_wt", sheet = sheet, skip = skip)
      
      output$error_m_5 <- renderText({
        validate(need(
          class(dat_w())!="try-error",
          attr(dat_w(), "condition")$message))
      })
      
      observeEvent(length(dat_w()) > 0, { 
#cat("preprocessMod\n observe desc_ebv ");cat(length(desc_ebv()), "\n")
        output$sanity_message <- renderText({
          if(!"desc_ev" %in% names(val)) {
            "Need to upload an EV description file first. Please reload the app and try again."
          } else if (!"dat_ev" %in% names(val)) {
            "Need to upload an EV file first. Please reload the app and try again."
          }
        })
        txt <- sanityCheckWt(dat_w(), dat_ev())
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$dat_w <- dat_w()
        } else {
          flag <- T
      #    shinyjs::delay(30000, stopApp())
        }
      })
      
      observeEvent(length(val$desc_ebv) > 0 && length(val$desc_ev) > 0 && 
                   length(val$dat_ebv) > 0 && length(val$dat_ev) > 0, {
        
        req(length(val$desc_ebv) > 0 && length(val$desc_ev) > 0 && 
              length(val$dat_ebv) > 0 && length(val$dat_ev) > 0,
            is.null(input$sanity_message))
        
        output$sanity_message <- renderText({
        "All good. Now you can move to Results, or Filter if you want to subset your inputs."
        })
      })

     # return(flag)
    })}
