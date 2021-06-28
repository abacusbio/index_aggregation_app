#'Plot a histogram for continuous variables
#'
#' @param df a reactive function with a data.frame in it
plotHist <- function(input, output, session,
                     df, xvar, group1, group2 = NULL, nbins = 30, # reactive(30),
                     font_size = reactive(12),...) {
# cat("plotHist\n ", xvar, "group1:", group1, "group2:", group2, "df:", class(df), "\n")
# print(head(df))

  if(class(df)[1]!="data.frame") df <- as.data.frame(df) # doesn't plot with tbl_df/tbl possibly from tidyr
  df[,group1] <- as.factor(df[,group1])
  # df[,group2] <- as.factor(df[,group2])
 
  p <- ggpubr::gghistogram(df, x = xvar, 
                           bins = nbins, alpha = 0.5,
                           add = "mean", rug = TRUE, 
                           color = group1, fill = group1,
                           palette = "npg",          # npg journal color palett. see ?ggpar
                           font.x = c(font_size(), "plain", "black"), # xlab
                           font.y = c(font_size(), "plain", "black"), # ylab
                           font.legend = c(font_size(), "plain", "black"),
                           panel.labs.font = 
                             list(face = "plain", color = "black", size = font_size(), angle = NULL), # facet label
              ...)
  
  if(length(unique(df[[group1]])) > 1 && length(unique(df[,group2])) > 1) { # df[[NULL]] causes error
    p <- ggpubr::facet(p, facet.by = c(group1, group2), scales = "free_y")
    
  } else if(length(unique(df[[group1]])) > 1) {
    p <- ggpubr::facet(p, facet.by = group1, scales = "free_y")
    
  } else if(length(unique(df[,group2])) > 1) {
    p <- ggpubr::facet(p, facet.by = group2, scales = "free_y")
  }
  
  return(p)
}

#'Plot a dot chart for discrete variables
#'
#' @param df a creactive function with a data.frame in it
plotLolipop <- function(input, output, session,
                    df, xvar, yvar, group1, group2 = NULL, 
                    font_size = reactive(12),...) {
cat("plotDot\n ", xvar, "group1:", group1, "group2:", group2, "df:", class(df), "\n")
# print(head(df))
  if(class(df)[1]!="data.frame") df <- as.data.frame(df) # doesn't plot with tbl_df/tbl possibly from tidyr
  df[,group1] <- as.factor(df[,group1])
  
  p <- ggpubr::ggdotchart(df, x = xvar, y = yvar, color = group1, group = group1,
                          palette = "npg",          # npg journal color palett. see ?ggpar
                          sorting = "descending", 
                          add = "segments", add.params = list(color = "lightgray", size = 1),
                          rotate = T, dot.size = 6, 
                          label = yvar, 
                          font.label = list(color = "white", size = font_size(), vjust = 0.5),
                          font.x = c(font_size(), "plain", "black"), # xlab
                          font.y = c(font_size(), "plain", "black"), # ylab
                          font.legend = c(font_size(), "plain", "black"),
                          panel.labs.font = 
                           list(face = "plain", color = "black", size = font_size(), angle = NULL), # facet label
                           ...)
  
  if(length(unique(df[[group1]])) > 1 && length(unique(df[,group2])) > 1) { # df[[NULL]] causes error
    p <- ggpubr::facet(p, facet.by = c(group1, group2), scales = "free_y")
    
  } else if(length(unique(df[[group1]])) > 1) {
    p <- ggpubr::facet(p, facet.by = group1, scales = "free_x")
    
  } else if(length(unique(df[,group2])) > 1) {
    p <- ggpubr::facet(p, facet.by = group2, scales = "free_x")
  }
  
  return(p)
}