#' Plot the correlation as sorted dots
#' @param m the correlation matrix, numeric
#' @param agg_index_names a reactive object within which a string scalar or vector, the aggregated
#'        index name(s)
#' @param n an integer, the number of bars per single plot  
#' @param show_n an integer, the total number of bars to show      
#' @return either a ggplot object or a list of ggplot object
plotcorrDot <- function(input, output, session,
                        m, agg_index_names = reactive(NULL), font_size = reactive(12),
                        # n = 30, show_n = reactive(10), 
                        ...) {
  
  m <- data.frame(m[, agg_index_names(), drop = F]) %>% 
    dplyr::mutate(Index = rownames(m)) %>% 
    dplyr::filter(!Index %in% agg_index_names()) %>% 
    tidyr::pivot_longer(-Index, names_to = "aggregated_index", values_to = "correlation")
  # Index aggregated_index correlation
  # index1 new_index1      0.5
  # index2 new_index1      0.4
  # ...
  # index1 new_index2      0.9
  # ...
# cat("plotCorrDot\n, m:", class(m));print(dim(m));print(head(m))
  
  # create an index for sorting
  m <- do.call(rbind, lapply(unique(m$aggregated_index), function(agg_index) {
    df <- dplyr::filter(m, aggregated_index == agg_index) %>% 
      dplyr::arrange(desc(correlation)) %>% 
      mutate(id = dplyr::row_number())
  }))
# cat(" agg_index_names:"); print(agg_index_names())
  
  p <- ggpubr::ggscatter(m, x = "id", y = "correlation",
                        color = "aggregated_index", alpha = 0.5, 
                        palette = "npg",         # npg journal color palett. see ?ggpar
                        sort.val = "desc",          # Sort the value in dscending order
                       # sort.by.groups = T,     # Don't sort inside each group
                        xlab = "sorted original index", 
                        font.y = c(font_size(), "plain", "black"), # y lab
                        ...) +
    ggpubr::rremove("x.ticks") + ggpubr::rremove("x.text")
  return(list(p = p, df = m))
}


#' #' Generic bar plot using ggpubr template
#' #' @param m the data.frame to plot
#' #' @param xvar a string, the x axis variable name
#' #' @param agg_index_names a reactive object within which a string scalar or vector, the aggregated
#' #'        index name(s)
#' #' @param n an integer, the number of bars per single plot        
#' #' @return either a ggplot object or a list of ggplot object
#' plotBarGivenNbars <- function(m, title, xvar = "Index", yvar = "correlation",
#'                               font_size = 12, n = 30, ...) {
#'   
#'   x.text.angle <- ifelse(nrow(m) > 10, 90, 0)
#'   
#'   if(nrow(m) <= n) {
#'     ps <- ggpubr::ggbarplot(m, x = xvar, y = yvar,
#'                             fill = ggpubr::get_palette("npg", k = 1), #"Index",
#'                             color = "white", 
#'                             palette = "npg",            # npg journal color palett. see ?ggpar
#'                             sort.val = "desc",          # Sort the value in dscending order
#'                             # sort.by.groups = FALSE,     # Don't sort inside each group
#'                             wt = NULL, # doesn't work
#'                             x.text.angle = x.text.angle, font.y = c(font_size, "plain", "black"), # y lab
#'                             font.tickslab = c(font_size, "plain", "black"),
#'                             ...)
#'   } else {
#'     
#'     idx <- seq(from = 1, to = nrow(m), by = n)
#'     
#'     ps <- lapply(seq(idx), function( i ) {
#'       
#'       if(i == length(idx)) {
#'         dff <- m[idx[i]:nrow(m), ]
#'       } else {
#'         dff <- m[idx[i]:idx[i+1], ]
#'       }
#'       
#'       return(ggpubr::ggbarplot(dff, x = xvar, y = yvar,
#'                                fill = ggpubr::get_palette("npg", k = 1), #"Index", 
#'                                color = "white", # Set bar border colors to white
#'                                palette = "npg",            # npg journal color palett. see ?ggpar
#'                                # sort.val = "desc",          # Sort the value in dscending order
#'                                # sort.by.groups = FALSE,     # Don't sort inside each group 
#'                                wt = NULL,
#'                                title = title,
#'                                x.text.angle = x.text.angle, font.y = c(font_size, "plain", "black"), # y lab 
#'                                font.tickslab = c(font_size, "plain", "black")))
#'     })
#'   } # if less than 30 old indexes
#'   return(ps)
#' }
#' 
#' 
#' #' Plot the correlation as sorted bars
#' #' @param m the correlation matrix, numeric
#' #' @param agg_index_names a reactive object within which a string scalar or vector, the aggregated
#' #'        index name(s)
#' #' @param n an integer, the number of bars per single plot  
#' #' @param show_n an integer, the total number of bars to show      
#' #' @return either a ggplot object or a list of ggplot object
#' plotcorrBar <- function(input, output, session,
#'                         m, agg_index_names = reactive(NULL), font_size = reactive(12), n = 30,
#'                         show_n = reactive(10)) {
#' 
#'   m <- data.frame(m[, agg_index_names(), drop = F]) %>% 
#'     dplyr::mutate(Index = rownames(m)) %>% 
#'     dplyr::filter(!Index %in% agg_index_names())
#' cat("plotCorrBar\n, m:", class(m));print(dim(m))#;print(head(m[,1:min(3, ncol(m))])  )
#' # print(sapply(m, class))
#' cat(" agg_index_names:"); print(agg_index_names())
#'   
#'   if(length(agg_index_names())==1) {
#'     
#'     names(m)[1] <- "correlation"
#'     m <- dplyr::arrange(m, desc(correlation)) %>% 
#'       dplyr::slice_head(n = show_n())
#'     
#'     ps <- plotBarGivenNbars(m, agg_index_names(), xvar = "Index", yvar = "correlation",
#'                             font_size = font_size())
#'     
#'   } else {
#'     
#'     ps <- lapply(agg_index_names(), function(agg_index_name) {
#'       
#'       m_sub <- m
#'       names(m_sub)[which(names(m_sub)==agg_index_name)] <- "correlation"
#'       m_sub <- dplyr::arrange(m_sub, desc(correlation)) %>% 
#'         dplyr::slice_head(n = show_n())
#'       
#'       p <- plotBarGivenNbars(m_sub, agg_index_name, xvar = "Index", yvar = "correlation",
#'                              font_size = font_size())
#'       return(p)
#'     }) # ps
#'     
#'     if(class(ps[[1]])[1]=="list") { # if list inside list
#'       ps <- unlist(ps, recursive = F) # flatten out to depth=1
#' # cat("   ps unlist:", length(ps));print(unique(sapply(ps, class)));print(ps[[1]])
#'     }
#'   } # if only 1 agg_index_name
#'   return(ps)
#' }

#' Plot the top n individual overlap across indexes
#' @param l a list. Each element is a data.frame of columns order, Index, plant and value (index 
#'        value)
#' @param agg_index_name a reactive object within which a string scalar or vector, the aggregated
#'        index name(s)
#' 
plotTopNdot <- function(input, output, session,
                        l, agg_index_name = reactive(NULL), font_size = reactive(12),
                        ...) {
# cat("plotTopNdot\n");cat(" agg_index_name:");print(agg_index_name());cat("  l name:", length(l));
# print(head(names(l)))
  ref_indexes <- grep("new_index", agg_index_name(), value = T)
# cat(" ref_indexes:");print(ref_indexes)
# cat(" l[[]]:\n");print(head(l[[1]])  )
  by_ref_index <- lapply(ref_indexes, function(ref_index) {
    plant_names <- l[[ref_index]]$plant # reference plants
# cat(" plant_names:");print(head(plant_names))    
    df <- do.call(rbind, lapply(l, function(df) {
      return(data.frame(Index = df$Index[1], aggregated_index = ref_index,
                        n =  sum(df$plant %in% plant_names, na.rm = T),
                        percent = sum(df$plant %in% plant_names, na.rm = T)/length(plant_names)*100))
    })) %>% arrange(desc(n)) %>% 
      dplyr::mutate(id = dplyr::row_number())
    
    # remove ref_index rows
    idx <- which(df$Index %in% ref_indexes)
    if(length(idx) > 0) df <- df[-idx,]
# cat("  df:\n");print(head(df)  )
    return(df)
  })
  # Index   aggregated_index    n   percent id
  # index1    new_index1       10   100     1
  # index2    new_index1       8    80      2
  # ...
  # index1    new_index2       5    50      1
  # ...
# cat(" by_ref_index:");print(head(by_ref_index[[1]]))
    p <- ggpubr::ggscatter(do.call(rbind, by_ref_index), x = "id", y = "percent",
                           color = "aggregated_index", alpha = 0.5,
                           palette = "npg",            # npg journal color palett. see ?ggpar
                           sort.val = "desc",          # Sort the value in dscending order
                           # sort.by.groups = T,     # Don't sort inside each group
                           xlab = "sorted original index", 
                           font.y = c(font_size(), "plain", "black"), # y lab
                           ...) +
      ggpubr::rremove("x.ticks") + ggpubr::rremove("x.text")
  
 return(list(p = p, df = do.call(rbind, by_ref_index)))
}