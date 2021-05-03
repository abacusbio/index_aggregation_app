preprocessUploadModsidebarUI <- function(id, title = "Step 1 file uploads") {
  ns <- NS(id)
  tagList(
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
    span(textOutput(ns("demo_message")), style = "color:orange"),
    # textOutput(ns("demo_message")),
    verbatimTextOutput(ns("sanity_message")) 
  )
}

#'@param data_name A string. The name for reactive value \code{val} to save the outputed value
#'@param clean logic. If True then apply cleaning function. Default is \code{False}.
#'@param type A string. Options are "survey", "1000minds" or "rank". This determines the cleaning
#'       function to apply to the input data. Only use when \code{clean} == \code{True}.
#'
preprocessUploadMod <- function(id, val, # data_name = "data", clean = T, type = "survey",
                                sheet = 1, skip = 0, desc_ebv = reactive(NULL),
                                ...) {
  moduleServer(
    id,
    function(input, output, session) {
# cat("preprocessMod\n");
      flag <- F
      
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
        } else {
          flag <- T
          delay(30000, stopApp())
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
        } else {
          flag <- T
          delay(30000, stopApp())
        }
      })
      
      ## EBV file
      dat_ebv <- uploadTableModuleServer("dat_ebv", sheet = sheet, skip = skip)
      
      output$error_m_3 <- renderText({
        validate(need(
          class(dat_ebv())!="try-error",
          attr(dat_ebv(), "condition")$message))
      })
      
      observeEvent(length(dat_ebv()) > 0, { 
# cat(" observe dat_ebv\n")
        output$sanity_message <- renderText({
          if(!"desc_ebv" %in% names(val)) "Please upload an EBV description file first."
        })
        txt <- sanityCheckEBV(dat_ebv(), desc_ebv())
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$dat_ebv <- dat_ebv()
        } else {
          flag <- T
          delay(30000, stopApp())
        }
      })
      
      ## EV file
      dat_ev <- uploadTableModuleServer("dat_ev", sheet = sheet, skip = skip)
      
      output$error_m_4 <- renderText({
        validate(need(
          class(dat_ev())!="try-error",
          attr(dat_ev(), "condition")$message))
      })
      
      observeEvent(length(dat_ev()) > 0, { 
        #cat("preprocessMod\n observe desc_ebv ");cat(length(desc_ebv()), "\n")
        output$sanity_message <- renderText({
          if(!"desc_ev" %in% names(val)) "Please upload an EBV description file first."
        })
        txt <- sanityCheckEV(dat_ev(), desc_ev())
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$dat_ev <- dat_ev()
        } else {
          flag <- T
          delay(30000, stopApp())
        }
      })
      
      ## EV weight file
      dat_wt <- uploadTableModuleServer("dat_wt", sheet = sheet, skip = skip)
      
      output$error_m_5 <- renderText({
        validate(need(
          class(dat_wt())!="try-error",
          attr(dat_wt(), "condition")$message))
      })
      
      observeEvent(length(dat_wt()) > 0, { 
        #cat("preprocessMod\n observe desc_ebv ");cat(length(desc_ebv()), "\n")
        output$sanity_message <- renderText({
          if(!"desc_ev" %in% names(val)) {
            "Please upload an EBV description file first."
          } else if (!"dat_ev" %in% names(val)) {
            "Please upload an economic value file first."
          }
        })
        txt <- sanityCheckWt(dat_wt(), dat_ev(), desc_ev()) 
        output$sanity_message <- renderText({ txt })
        if(is.null(txt)) {
          val$dat_wt <- dat_wt()
        } else {
          flag <- T
          delay(30000, stopApp())
        }
      })
      
      observeEvent(length(desc_ebv()) > 0 && length(desc_ev()) > 0 && 
                   length(dat_ebv()) > 0 && length(dat_ev()) > 0, {
        
        req(#length(desc_ebv()) > 0, length(desc_ev()) > 0, 
            #length(dat_ebv()) > 0, length(dat_ev()) > 0,
            is.null(input$sanity_message))
        
        output$sanity_message <- renderText({
        "All good. Now you can move to Results, or Filter if you want to subset your inputs."
        })
      })
# cat("preprocessMod\n flag", flag, "\n")  
     # return(flag)
    })}
