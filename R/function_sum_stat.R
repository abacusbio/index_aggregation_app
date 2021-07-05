#'Plot a histogram for continuous variables
#'
#' @param df a reactive function with a data.frame in it
#' @param scales a string. Default is "free_y" for \code{ggpubr::facet}. Can also be "fixed", 
#'        "free_x" or "free_both". When "free_both" then return a list of plots.
#' @param xlab a character string to show on xlab. Default is \code{xvar}.
#' 
#' @return when scales == "free_both", return a list of ggpubr objects, otherwise return a ggpubr
#'         object.
plotHist <- function(input, output, session,
                     df, xvar, group1, group2 = NULL, nbins = 30, # reactive(30),
                     font_size = reactive(12), scales = "free_y", xlab = xvar, ...) {
# cat("plotHist\n ", xvar, "group1:", group1, "group2:", group2, "df:", class(df), "scales:", scales,
    # "\n"); # print(head(df))
  if(class(df)[1]!="data.frame") df <- as.data.frame(df) # doesn't plot with tbl_df/tbl possibly from tidyr
  df[,group1] <- as.factor(df[,group1])
  # df[,group2] <- as.factor(df[,group2])
  if(is.null(xlab)) xlab <- xvar
  
  if(scales == "free_both" &&
     (length(unique(df[[group1]])) > 1 && length(unique(df[,group2])) <= 1)) {
    
    ps <- lapply(unique(df[[group1]]), function(i) {
      ggpubr::gghistogram(df[df[,group1]==i, ], x = xvar, 
                          bins = nbins, alpha = 0.5,
                          add = "mean", rug = TRUE, 
                          xlab = xlab, title = i, 
                          color = "#008b99", fill = "#008b99", #group1,
                          #palette = "npg",          # npg journal color palett. see ?ggpar
                          font.x = c(font_size(), "plain", "black"), # xlab
                          font.y = c(font_size(), "plain", "black"), # ylab
                          ...)
    })
    return(ps)
  } else {
    
    if(scales == "free_both") scales <- "free_y"
    p <- ggpubr::gghistogram(df, x = xvar, 
                             bins = nbins, alpha = 0.5,
                             add = "mean", rug = TRUE, 
                             xlab = xlab,
                             color = group1, fill = group1, #group1,
                             #palette = "npg",          # npg journal color palett. see ?ggpar
                             font.x = c(font_size(), "plain", "black"), # xlab
                             font.y = c(font_size(), "plain", "black"), # ylab
                             font.legend = c(font_size(), "plain", "black"),
                             panel.labs.font = 
                               list(face = "plain", color = "black", size = font_size(), angle = NULL), # facet label
                             ...)
    
    if(length(unique(df[[group1]])) > 1 && length(unique(df[,group2])) > 1) { # df[[NULL]] causes error
      p <- ggpubr::facet(p, facet.by = c(group2, group1), scales = scales)
      
    } else if(length(unique(df[[group1]])) > 1) {
      p <- ggpubr::facet(p, facet.by = group1, scales = scales)
      
    } else if(length(unique(df[,group2])) > 1) {
      p <- ggpubr::facet(p, facet.by = group2, scales = scales)
    }  
    return(p)
  } # if free_both
}

#'Plot a dot chart for discrete variables
#'
#' @param df a creactive function with a data.frame in it
#' @param scales a string. Default is "free_y" for \code{ggpubr::facet}. Can also be "fixed", 
#'        "free_x"
plotLolipop <- function(input, output, session,
                    df, xvars, yvar, group1, group2 = NULL, 
                    font_size = reactive(12), scales = "free_y",...) {
# cat("plotDot\n ", xvars, "group1:", group1, "group2:", group2, "df:", class(df), "\n")
# print(head(df))
  if(class(df)[1]!="data.frame") df <- as.data.frame(df) # doesn't plot with tbl_df/tbl possibly from tidyr
  df[,group1] <- as.character(df[,group1])
  
  # expand colors to more than 10 levels
  cols <- ggsci::pal_npg(alpha = 0.5)(8)
  
 # if(length(unique(df[["variable"]])) > 1) {
# cat("  group1==variable && length(group1) > 1\n")
    ps <- lapply(unique(df[["variable"]]), function(var1) {

      dff <- filter(df, variable == var1) # across(all_of(group1), ~ .x==var1))
      
      if(group1=="variable") { # no group_var. group1=="variable". group2==NULL
        if(length(dff[[xvars]]) > 8) {
          cols <- colorRampPalette(ggsci::pal_npg(alpha = 0.5)(8))(length(dff[[xvars]]))  
        }
# cat("   cols len:", length(cols), "\n")
        return(ggpubr::ggdotchart(dff, x = xvars, y = yvar, 
                                  palette = cols,          # npg journal color palett. see ?ggpar
                                  color = xvars, #group = group1,
                                  sorting = "descending",
                                  add = "segments", add.params = list(color = "lightgray", size = 1),
                                  # rotate = T, 
                                  label = yvar, title = var1,
                                  dot.size = font_size()/1.5, 
                                  font.label = list(color = "black", size = font_size(), vjust = 0.5),
                                  font.x = c(font_size(), "plain", "black"), # xlab
                                  font.y = c(font_size(), "plain", "black"), # ylab
                                  font.legend = c(font_size(), "plain", "black"),
                                  ...))
      } else {                 # group_var exist. group2=="variable" group1==group_var
        if(length(unique(dff[[group1]])) > 8) {
          cols <- colorRampPalette(ggsci::pal_npg(alpha = 0.5)(8))(length(unique(dff[[group1]])))  
        }
        
        return(ggpubr::ggdotchart(dff, x = xvars, y = yvar, 
                                 palette = cols,          # npg journal color palett. see ?ggpar
                                 color = group1, group = group1,
                                 position = position_dodge(1), sorting = "descending",
                                 add = "segments", add.params = list(color = "lightgray", size = 1),
                                 # rotate = T, 
                                 title = var1, # label = yvar, 
                                 dot.size = font_size()/1.5, 
                                 font.label = list(color = "black", size = font_size(), vjust = 0.5),
                                 font.x = c(font_size(), "plain", "black"), # xlab
                                 font.y = c(font_size(), "plain", "black"), # ylab
                                 font.legend = c(font_size(), "plain", "black"),
                                 panel.labs.font = 
                                   list(face = "plain", color = "black", size = font_size(), angle = NULL), # facet label
                                 ...))
        #if(!is.null(group2)) { # group2=="variable" group1==group_var
        #  return(ggpubr::facet(p, facet.by = group2, ncol = 1, scales = scales))
        #}    
      } # if group1=="variable"
    }) # lapply
  #}
  return(ps)
}