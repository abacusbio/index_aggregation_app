
#' Sanity check the uploaded description file format
#' @param df_description a string, the EBV description data.frame name, e.g. desc_ebv
sanityCheckEBVdesc <- function(df_description) {
  
  ## illegal header other than the 5 headers listed below
  if(sum(is.na(match(colnames(df_description),
                     c("column_labelling", "classifier", "unit", "group", "order")))) > 0) {
    out <- paste0("Breeding values: file description error:\n",
                  " Illegal header. Please use 'column_labelling' and 'classifier'.\n",
                  " Your header is ", paste0(colnames(df_description), collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out) # This will be assigned to flag variable
  }
  
  ## duplicated header
  if(sum(duplicated(colnames(df_description))) > 0) {
    out <- paste0("Breeding values: file description error:\n",
             " Duplicated headers ",
             colnames(df_description)[duplicated(colnames(df_description))],
             "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
    
  ## "ID" not exist
  if(!"ID" %in% df_description$column_labelling) {
    out <- paste0("Breeding values: file description error:\n",
                  "At least one unique ID header has to be 'ID'.\n",
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## Missing classifier
  m <- match(c("EBV", "ID"), df_description$classifier)
  if(sum(is.na(m)) > 0) {
    out <- paste0("Economic values: file description error:\n",
                  " Missing classifier:\n",
                  paste0(df_description$classifier[which(is.na(m))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## unknown classifier
  m <- !grepl("EBV|Group|ID|ClassVar", df_description$classifier)
  if(any(m)) {
    out <- paste0("Economic values: file description error:\n",
                  " Unknown classifier:\n",
                  paste0(df_description$classifier[m], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## input errors
  traits <- df_description$column_labelling[df_description$classifier=="EBV"]
  accs <- df_description$column_labelling[df_description$classifier=="ACC"]
  
  if(length(accs) > 0) { # some inputs do not have ACC
    # if(length(unique(diff(match(unlist(gsub("ACC", "", accs, ignore.case = T)),
    #                             traits)))) > 1) {  # traits and accs in different sequence
    #   output$sanity <- renderText({
    #     paste0("Breeding values: file description error:          ",
    #            "Trait and accuracy names are not ordered.          ")
    #   })
    #   return(T)
    # }
    
    if(sum(is.na(match(unlist(gsub("ACC", "", accs, ignore.case = T)),
                       traits))) > 0) {
      out <- paste0("Breeding values: file description error:\n",
               " Accuracy names do not match trait names.
               \n The app will sleep in 30,000 seconds. Please reload and try again.")
    }
    return(out)
  }
  
  order <- as.numeric(na.omit(df_description$order))
  if(length(order) > 0) {
    dif <- diff(sort(order))
    if(length(which(dif ==0)) > 0) {
      out <- paste0("Breeding values: file description error:\n",
               " Warning: there are identical order numbers.\n The program will run
               as it is but you can change your order if that matters.\n 
               The app will sleep in 30,000 seconds. Please reload and try again.")
      return(out)
    }
    
    m <- match(which(df_description$classifier=="EBV"),
               which(!df_description$order %in% c("", NA)))
    if(sum(is.na(m)) > 0) {
      out <- paste0("Breeding values: file description error:\n",
               " Missing order for trait(s)\n",
               paste0(df_description$column_labelling[df_description$classifier=="EBV"][is.na(m)],
                      collapse = ", "),
               "\n The app will sleep in 30,000 seconds. Please reload and try again.")
      return(out)
    }
  } # if order exists
  
  group <- which(!df_description$group %in% c("", NA))
  if(length(group) > 0)  {
    m <- match(which(df_description$classifier=="EBV"), group)
    if(sum(is.na(m)) > 0) {
     out <- paste0("Breeding values: file description error:\n",
               " Missing group for trait(s) ",
               paste0(df_description$column_labelling[df_description$classifier=="EBV"][is.na(m)],
                      collapse = ", "),
               "\n The app will sleep in 30,000 seconds. Please reload and try again.")
     return(out)
    }
  } # if group exists
}

#' @param df_description a string, the EV description file name e.g. desc_ev
#' @param desc_ebv a string, the EBV description file name
sanityCheckEVdesc <- function(df_description, desc_ebv = NULL) {
  # if(is.null(desc_ebv)) {
  #   return("Please upload EBV description file first.")
  # }
  
  ## illegal header other than the 5 headers listed below
  if(sum(is.na(match(colnames(df_description),
                     c("column_labelling", "classifier")))) > 0) {
    out <- paste0("Economic values: file description error:\n",
                  " Illegal header. Please use 'column_labelling' and 'classifier'.\n",
                  " Your header is ", paste0(colnames(df_description), collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## duplicated header
  if(sum(duplicated(colnames(df_description))) > 0) {
    out <- paste0("Economic values: file description error:\n",
                  " Duplicated headers ",
                  colnames(df_description)[duplicated(colnames(df_description))],
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## "Index" not exist
  if(!"Index" %in% df_description$column_labelling) {
    out <- paste0("Economic values: file description error:\n",
                  "At least one unique ID header has to be 'Index'.",
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## Missing classifier
  m <- match(c("EV", "ID"), df_description$classifier)
  if(sum(is.na(m)) > 0) {
    out <- paste0("Economic values: file description error:\n",
                  " Missing classifier:\n",
                  paste0(df_description$classifier[which(is.na(m))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## unknown classifier
  m <- !grepl("EV|Group|ID|ClassVar", df_description$classifier)
  if(any(m)) {
    out <- paste0("Economic values: file description error:\n",
                  " Unknown classifier:\n",
                  paste0(df_description$classifier[m], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## trait names in EV and EBV descriptions do not match
  t_ev <- df_description$column_labelling[df_description$classifer=="EV"]
  t_ebv <- desc_ebv$column_labelling[desc_ebv$classifer=="EBV"]
  m <- match(t_ev, t_ebv)
  if(sum(is.na(m)) > 0) {
    out <- paste0("Economic values: file description error:\n",
                  " Trait(s) in EV description doesn't exist in EBV description:\n",
                  paste0(df_description$column_labelling[which(is.na(m))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
}

#' @param df_description a string, the EBV description file name
sanityCheckEBV <- function(df_ebv, df_description = NULL) {
#cat("sanityCheckEBV\n df_description: ");print(is.null(df_description))  
  ## duplicated header
  if(sum(duplicated(colnames(df_ebv))) > 0) {
    out <- paste0("Breeding values: data error:\n",
                  " Duplicated headers ",
                  paste0(colnames(df_ebv)[duplicated(colnames(df_ebv))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again."
    )
    return(out)
    }
  
  m <- match(df_description$column_labelling,
             unlist(gsub("EBV", "", colnames(df_ebv), ignore.case = T)))
  if(sum(is.na(m)) > 0) {
    output <- paste0("Breeding value column header do not match file description:\n",
             "'Breeding values: file description' column_labelling is:\n",
             paste0(df_description$column_labelling, collapse = ", "),
             "\n'Breeding values: data' column headers are:\n",
             paste0(colnames(df_ebv), collapse = ", "),
             "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
    }
  
  m <- match(unlist(gsub("EBV", "", colnames(df_ebv), ignore.case = T)),
             df_description$column_labelling)
  if(sum(is.na(m)) > 0) {
    out <- paste0("Breeding values: data error:\n",
             " header(s) ",
             paste0(colnames(df_ebv)[which(is.na(m))], collapse = ", "),
             " does not exist in description file.\n
           The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
    }
  
  ## EBV and ACC are not numerical
  traits <- colnames(df_ebv)[df_description$classifier=="EBV"]
  accs <- colnames(df_ebv)[df_description$classifier=="ACC"]
  
  classes <- unlist(lapply(c(traits, accs), function( i ){
    class(df_ebv[, i])
  }))
  
  if(any(!classes %in% c("numeric", "double", "integer"))) {
    
    out <- paste0("Breeding values: data error:\n",
             " Character strings detected in\n",
             paste0(c(traits, accs)[which(!classes %in% c("numeric", "double", "integer"))],
                    collapse = ", "),
             "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
    }
}

#' Note that this is a transpose of the EV file in index testing app.
#' @param df_description a string, the EV description file name
sanityCheckEV <- function(df_econval, df_description) {
  
  if(colnames(df_econval)[ 1 ]!="Index") {
    out <- paste0("Economic values: data error:\n",
             " First column name should be 'Index'.\n 
      The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## duplicated header
  if(sum(duplicated(colnames(df_econval))) > 0) {
    out <- paste0("Economic values: data error:\n",
                  " Duplicated headers ",
                  paste0(colnames(df_econval)[duplicated(colnames(df_econval))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again."
    )
    return(out)
  }
  
  m <- match(df_description$column_labelling,
             unlist(gsub("EV", "", colnames(df_econval), ignore.case = T)))
  if(sum(is.na(m)) > 0) {
    output <- paste0("Economic value column header do not match file description:\n",
                     "'Economic values: file description' column_labelling(s) is:\n",
                     paste0(df_description$column_labelling, collapse = ", "),
                     "\n'Economic values: data' column header(s) is:\n",
                     paste0(colnames(df_econval), collapse = ", "),
                     "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  m <- match(unlist(gsub("EV", "", colnames(df_econval), ignore.case = T)),
             df_description$column_labelling)
  if(sum(is.na(m)) > 0) {
    out <- paste0("Economic values: data error:\n",
                  " header(s) ",
                  paste0(colnames(df_econval)[which(is.na(m))], collapse = ", "),
                  " does not exist in description file.\n
           The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
    }
  }

sanityCheckWt <- function(df_wt, desc_ev = NULL, df_econval = NULL) {
  
  # if(is.null(df_econval)) {
  #   return("Please upload an economic value file first.")
  # }
  # 
  # if(is.null(desc_ev)) {
  #   return("Please upload an economic value description file first.")
  # }
  
  ## Weights are not numerical
  traits <- desc_ev$column_labelling[desc_ev$classifier %in% c("ClassVar", "EV")]
  w <- grep("[^Index]", colnames(df_wt))
  
  classes <- unlist(lapply(w, function( i ){
    class(df_wt[, i])
  }))
  
  if(any(!classes %in% c("numeric", "double", "integer"))) {
    
    out <- paste0("Aggregation weight: data error:\n",
                  " Character strings detected in\n",
                  paste0(w[which(!classes %in% c("numeric", "double", "integer"))],
                         collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
  
  ## names do not match
  m <- match(w, traits)
  if(any(is.na(m)) > 0) {
    out <- paste0("Aggregation weight: data error:\n",
                  " Variable name(s) does not exist in economic value files:\n",
                  paste0(w[which(is.na(m))], collapse = ", "),
                  "\n The app will sleep in 30,000 seconds. Please reload and try again.")
    return(out)
  }
}

#' Trim irregular symbols in a vector
#'
#' Only allow numbers, characters, punctuates and white space
#'
#' @param v a string vector
#' @source {https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html#:~:text=A%20'regular
#'         %20expression'%20is%20a,use%20a%20literal%20regular%20expression}
#' @return a string vector
trimIrregularSymbols <- function(v) {
  sapply(v, gsub, pattern = "[^0-9A-Za-z[:punct:] ]", replace = "") %>%
    sapply(gsub, pattern = "[*^]+[ ]{2,}", replace = "") %>% # remove *#^ and white space >=2
    sapply(gsub, pattern = "/", replace = " or ") %>% # readRDS error
    sapply(gsub, pattern = "#", replace = "N") %>% # number of
    sapply(gsub, pattern = ",", replace = ";") # avoid problem saving as .csv
}

#' Trim irregular symbols in a data frame
#'
#' @param dat a data frame
#' @return a data frame
trimIrregularSymbolsData <- function(dat) {
  #cat("trimIrregularSymbolsData\n")
  dat1 <- lapply(1:ncol(dat), function( i ) {
    
    if(class(dat[[i]])[1] == "character") {
      #cat(i, class(trimIrregularSymbols(dat[,i, drop = F])), "\n")
      return(trimIrregularSymbols(dat[,i, drop = T])) # don't change. can cause problem
    } else {
      #cat(i, class(dat[,i, drop = F]), "\n")
      return(dat[,i, drop = T]) # don't change. can cause problem
    }
  })
  dat1 <- cbind.data.frame(dat1, stringsAsFactors = F)
  
  names(dat1) <- trimIrregularSymbols(names(dat))
  #print("dat1"); print(head(dat1))
  return(dat1)
}

#' Trim duplicated col name suffix
#' @describe if seen "...2" then this is a duplicated column name
#'
#' @param v string vector, the colnames of the .csv
trimDupSuffix <- function(v) {
  gsub("\\.{3}\\d+", "", v)
}
