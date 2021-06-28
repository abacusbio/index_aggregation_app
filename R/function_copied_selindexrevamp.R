#'Clean the description file input
#'@param df_description data.frame, == dt_description()
#'@return \item{df_description} a data.frame
cleanDescData <- function(df_description) {
  if("order" %in% colnames(df_description)) {
    df_description$order <- as.integer(df_description$order)
  }
  
  return(df_description)
}

#' Clean EBV input
#' @description Change the column class of EBV input file according to description file
#' and rescale accuracy columns.
#' 
#' @param df_description data.frame, the description input file
#' @param df_ebv data.frame, the EBV input file
#' @return a cleaned EBV data.frame
cleanEbvData <- function(df_description, df_ebv) {
  
  # change column classes
  idx <- grep(pattern = "ACC", x = df_description$classifier)
  if(length(idx)>0){                             # check accuracy columns exist
    acc_cols <- df_description$column_labelling[idx]
    
    # temp <- mutate_at(df_ebv, vars(acc_cols), list(~ifelse(is.na(.), -10, .))) # 5june2020
    temp <- df_ebv
    
    if(class(temp[,acc_cols[ 1 ]])=="character") { # why does this happen?
      for(i in acc_cols) {
        #  set(temp, j=i, value=as.numeric(temp[[ i ]])) # data.table syntax
        temp[,i] <- as.numeric(temp[, i])
        # print("character to numerical") # debug
        
        if(max(temp[[ i ]], na.rm = T) < 1) {       # change rel scaling
          # set(temp, j=i, value = temp[[ i ]]*100) # data.table syntax
          temp[,i] <- temp[, i]*100
        }
      }
    }
  } else { temp <- df_ebv } 
  
  idx <- grep("ClassVar|ID", df_description$classifier)
  
  if(length(idx)>0) {                          # check if classifier columns exists
    fixed_var_cols <- df_description$column_labelling[idx]
    
    for (k in fixed_var_cols) {
      # set(temp, j=k, value=as.character(temp[[k]]))
      temp[,k] <- as.character(temp[,k])
      # print("numerical to character") # debug
    }
  }
  
  # change order
  # if df_ebv colnames does not match column_labelling in df_description
  # it's likely someone concatenated "EBV" to df_ebv colnames
  if(sum(is.na(match(df_description$column_labelling, colnames(temp)))) > 0) {
    colnames(temp) <- unlist(gsub("EBV", "", colnames(temp), ignore.case = T))
  }
  temp <- select_at(temp, vars(df_description$column_labelling))
  
  return(temp)
}

#'@param cl a hclust() output
#'@param clcut a vector of grouping categories, with names as the trait names
#'@param circle a logical value indicating if the dendrogram need to be circluar (fan shape)
drawDendro <- function(cl, clcut, circle = F) {
#cat("drawDendro\n clcut:");#print(clcut)
  plot(cl, main = "", cex = 2, cex.lab = 1.5, cex.axis = 1.5)
  gg <- rect.hclust(cl, k = max(clcut), border = "red")
  clust.gr<-data.frame(num=unlist(gg),
                       clust=rep(paste0("clust", order(seq(length(gg)), decreasing = T)),
                                 times=sapply(gg,length)))
#cat(" names(clcut):");print(names(clcut));cat(" factor clcut:");print(factor(clcut))
  clcut1 <- data.frame(label = names(clcut), group = factor(clcut))
  dendr <- ggdendro::dendro_data(cl, type = "rectangle") 
  text.df<-merge(ggdendro::label(dendr),clust.gr,by.x="label",by.y="row.names")
  text.df <- merge(text.df, clcut1, by = "label")
  
  pDendro <- ggplot() + 
    geom_segment(data = ggdendro::segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_label(data=text.df, aes(x=x, y=y, label=label, hjust=0, fill=group), color = "black") +
    #scale_fill_brewer(palette = "Set3")
    ggsci::scale_fill_npg()
  #  theme_dendro() + 
  #  theme(legend.position = "None")
  
  if(circle) {
    pDendro <- pDendro +
      coord_polar(theta = "x") + scale_y_reverse(expand = c(0.2, 0)) +   
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    
  } else {
    pDendro <- pDendro +
      coord_flip() + scale_y_continuous(expand=c(0, 0.2)) +
      labs(y = "height")
  }
  
  pDendro <- pDendro +
    theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "None", #"top",
          panel.background=element_rect(fill="white"), panel.grid=element_blank(),
          text = element_text(size = 14)
      )
  
  return(pDendro)
}