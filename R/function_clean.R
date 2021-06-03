cleanEVplant <- function(desc_ev, dat_ev) {
  
  if(class(dat_ev$Index)!="character") dat_ev$Index <- as.character(dat_ev$Index)

  m <- desc_ev$column_labelling[desc_ev$classifier=="EV"]
  m <- which(names(dat_ev) %in% m)
  
  for(i in m) {
    dat_ev[, i][which(is.na(dat_ev[, i]))] <- 0
  }
  
  if(sum(!is.na(match(dat_ev$Index, colnames(dat_ev)))) > 0) { # index and trait name overlaps
    idx <- which(dat_ev$Index %in% colnames(dat_ev))
    dat_ev$Index[idx] <- paste0(dat_ev$Index[idx], "_index")
  }
  
  return(dat_ev)
}

cleanW <- function(dat_w) {
# cat("cleanW\n dat_w: ", class(dat_w), "\n");print(head(dat_w)) 
  if(class(dat_w$Index)!="character") dat_w$Index <- as.character(dat_w$Index)
  
  for(i in 2:ncol(dat_w)) {
    dat_w[,i][which(is.na(dat_w[,i]))] <- 0
  }
  
  return(dat_w)
}