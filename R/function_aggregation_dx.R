#' Convert a correlation matrix to a longer table for plotting
#' 
#' @param m the correlation matrix, numeric
#' @param agg_index_names a reactive object within which a string scalar or vector, the aggregated
#'        index name(s)
#' @return a data.frame with columns Index, aggregated_index, correlation, id
makeLongCor <- function(input, output, session,
                        m, agg_index_names = reactive(NULL)) {
# cat("makeLongCor\n, m:", class(m));print(dim(m));#print(head(m))
# cat(" agg_index_names:");print(agg_index_names())
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
  
  # create an index for sorting
  # Index aggregated_index correlation id
  m <- do.call(rbind, lapply(unique(m$aggregated_index), function(agg_index) {
    return(dplyr::filter(m, aggregated_index == agg_index) %>% 
      dplyr::arrange(desc(correlation)) %>% 
      mutate(id = dplyr::row_number()))
  }))
  
  return(m)
}

#' Plot the correlation as sorted dots
#' @param m the long correlation data.frame from \code{makeLongCor}. Columns are Index, 
#'        aggregated_index, correlation, ID
#' @param n an integer, the number of bars per single plot  
#' @param show_n an integer, the total number of bars to show      
#' @return either a ggplot object or a list of ggplot object
plotcorrDot <- function(input, output, session,
                        m, font_size = reactive(12), fixed_y_scale = reactive(T),
                        baseline = F,
                        # n = 30, show_n = reactive(10), 
                        ...) {
# cat("plotCorrDot\n, m:", class(m));print(dim(m));#print(head(m))
# cat("  c(min(0, m[['correlation']]), 1):");print( c(min(0, m[["correlation"]], na.rm = T), 1))
  if(fixed_y_scale()) {
    ylim <- c(min(0, m[["correlation"]], na.rm = T), 1)
  } else {
    ylim <- range(m[["correlation"]])
  }
  
  if(baseline) {
    palettes <- c("#808080", # avg_index
                  ggpubr::get_palette("npg", length(unique(df[,fill]))-1))
  } else { palettes = "npg"} 
  
  p <- ggpubr::ggscatter(m, x = "id", y = "correlation",
                         color = "aggregated_index", alpha = 0.5, 
                         palette = palettes,          # npg journal color palett. see ?ggpar
                         sort.val = "desc",        # Sort the value in dscending order
                         # sort.by.groups = T,     # Don't sort inside each group
                         ylim = ylim,
                         xlab = "sorted original index",
                         font.x = c(font_size(), "plain", "black"), # xlab
                         font.y = c(font_size(), "plain", "black"), # y lab
                         font.legend = c(font_size(), "plain", "black"),
                         ...) # +
   # ggpubr::rremove("x.ticks") + ggpubr::rremove("x.text")
  return(list(p = p, df = m))
}

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
                           # palette = tail(
                             # ggpubr::get_palette("npg", length(unique(df[,"aggregated_index"]))+1),
                             # -1), # skip red, the 1st color
                           sort.val = "desc",          # Sort the value in dscending order
                           # sort.by.groups = T,     # Don't sort inside each group
                           xlab = "sorted original index", 
                           font.x = c(font_size(), "plain", "black"), # xlab
                           font.y = c(font_size(), "plain", "black"), # y lab
                           font.legend = c(font_size(), "plain", "black"),
                           ...) +
      ggpubr::rremove("x.ticks") + ggpubr::rremove("x.text") # remove tick marks and tick text
  
 return(list(p = p, df = do.call(rbind, by_ref_index)))
}

#' Plot the classification variable distribution across indexes of a type
#'
#' @param df a data.frame of columns aggregated_by, aggregated_index, n, percent. e.g.
#'        classvar_summary() filtered by aggregated_by
#'
plotClassvarBar <- function(#input, output, session, 
                            df, x, fill = "aggregated_index", use_count, font_size = 12, #reactive(12),
                            ...) {
# cat("plotClassvarBar\n df:", class(df), " x:", x, " fill:", fill, " use_count:", use_count, "\n")
# print(head(df));#print(str(df))
  for(i in c(x, fill)) {
    if(class(df[,i])[1] %in% c("numeric", "integer", "double", "float")) {
      df[,i] <- as.factor(df[,i])
    }
  }

  if(use_count) { # ifelse doesn't work with vector well
    df$label <- df$n
  } else {
    df$label <- paste0(sapply(df$percent, format, digits = 2, nsmall = 0), "%")
  }
# cat(" df_label:");print(df$label)
  # if(input$switch_index_classvar) {

  # x <-input$class_var
  # fill <- "aggregated_index"
  # } else {

  # x <-"aggregated_index"
  # fill <- input$class_var
  # }
# cat("  x:", x, " fill:", fill, "\n")
  # ps <- lapply(unique(df$aggregated_by), function(agg_by) {
  # df_sub <- dplyr::filter(df, aggregated_by == agg_by)
  if(use_count) {
    y <- "n"; ylab <- "count"
    
    p <- ggpubr::ggbarplot(
      df, x = x, y = y, fill = fill, color = "white",
      # sort.val = "desc", sort.by.groups = T, # bug of duplicated factor levels
      position = position_stack(reverse = TRUE),
      label = T, lab.col = "white", lab.pos = "in", lab.hjust = 1, lab.vjust = -0.5,
      lab.size = max(3, font_size-8),
      # title = agg_by,
      #ylab = ylab,
      ...)
      
  } else {
    y <- "percent"; ylab <- "percent(%)"
    
    p <- ggpubr::ggbarplot(
      df, x = x, y = y, fill = fill, color = "white",
      # sort.val = "desc", sort.by.groups = T, # bug of duplicated factor levels
      position = position_stack(reverse = TRUE), # position_fill force range 0-1
      label = df$label, lab.col = "white", lab.pos = "in", lab.hjust = 1, lab.vjust = -0.5,
      lab.size = max(3, font_size-8),
      # title = agg_by,
      #ylab = ylab, 
      ...)
  }

  p <- ggpubr::ggpar(p, ylab = ylab, orientation = "horiz",
                     font.x = c(font_size, "plain", "black"), # xlab
                     font.y = c(font_size, "plain", "black"), # y lab
                     font.legend = c(font_size, "plain", "black"),
                     font.tickslab = c(font_size+2, "plain", "black"),
                     # font.label = list(size = font_size-1, face = "plain", color = "white"), # doesnt work
                     palette = ggpubr::get_palette("npg", length(unique(df[,fill]))))
    # ggpubr::set_palette(p, ggpubr::get_palette("npg", length(unique(df[,fill])))) +
    # ggpubr::rotate_x_text(angle = x.text.angle)
  return(p)
  #  })
}

#' Plot a continuous variable (weighting) distribution across indexes of a type
#'
#' @param df a data.frame of columns aggregated_by, aggregated_index, sum_[weight]. e.g.
#'        weight_summary() filtered by aggregated_by
#'
plotNumvarBar <- function(df, x, y, fill = "aggregated_index", use_count,
                          font_size = 12, digits = 0, ...) {
# cat("plotNumvarBar\n df:", class(df), " x:", x, " y:", y, " fill:", fill, " use_count:", use_count,
    # "df:\n"); print(head(df))
  for(i in c(x, fill)) {
    if(class(df[,i])[1] %in% c("numeric", "integer", "double", "float")) {
      df[,i] <- as.factor(df[,i])
    }
  }
# print(str(df))  
  if(use_count) { # ifelse doesn't work with vector well
    df$label <- round(df[,y], digits = digits)
  } else {
    df$label <- paste0(sapply(df[,y], round, digits = digits), "%")
  }
# cat(" label:");print(df$label)  
  if(use_count) {
    
    p <- ggpubr::ggbarplot(
      df, x = x, y = y, fill = fill, color = "white",
      # sort.val = "desc", sort.by.groups = T, # bug of duplicated factor levels
     # position = position_stack(reverse = TRUE), # position_fill force range 0-1
      label = df$label, lab.col = "white", lab.pos = "in", lab.hjust = 1, lab.vjust = -0.5,
      lab.size = max(3, font_size-8),
      # title = agg_by,
      ...)
    
  } else {
    
    xlab = paste0(y, "(%)")
    p <- ggpubr::ggbarplot(
      df, x = x, y = y, fill = fill, color = "white",
      # sort.val = "desc", sort.by.groups = T, # bug of duplicated factor levels
     # position = position_stack(reverse = TRUE), # position_fill force range 0-1
      label = df$label, lab.col = "white", lab.pos = "in", lab.hjust = 1, lab.vjust = -0.5,
      lab.size = max(3, font_size-8),
      # title = agg_by,
      ...)
  }
  
  p <- ggpubr::ggpar(p, ylab = F, legend = "none", orientation = "horiz",
                     font.x = c(font_size, "plain", "black"), # xlab
                     # font.y = c(font_size, "plain", "black"), # y lab
                     # font.legend = c(font_size, "plain", "black"),
                     font.tickslab = c(font_size+2, "plain", "black"),
                     palette = ggpubr::get_palette("npg", length(unique(df[,fill]))))
  
  return(p)
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