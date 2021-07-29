#'Plot a histogram for continuous variables
#'
#' @param df a reactive function with a data.frame in it
#' @param scales a string. Default is "free_y" for \code{ggpubr::facet}. Can also be "fixed", 
#'        "free_x" or "free_both". When "free_both" then return a list of plots.
#' @param xlab a character string to show on xlab. Default is \code{xvar}.
#' 
#' @return when scales == "free_both", return a list of ggpubr objects, otherwise return a ggpubr
#'         object.
plotBox <- function(input, output, session,
                     df, xvar, yvar, group = NULL, order = NULL,
                     font_size = reactive(12), xlab = xvar, ...) {
# cat("plotBox\n order:");print(head(order)  )
  if(is.null(group)) {
    fill <- "#008b99"
    palette <- "npg"
    df[[xvar]] <- factor(df[[xvar]], levels = order)
    
  } else {
    fill <- group
    palette <- ggpubr::get_palette("npg", length(unique(df[[fill]])))
  }
  
  return(ggpubr::ggboxplot(df, x = xvar, y = yvar,
                         order = order,
                         xlab = xlab,
                         alpha = 0.5, fill = fill, palette = palette,
                         orientation = "horiz", legend = "none",
                         font.x = c(font_size(), "plain", "black"), # xlab
                         font.y = c(font_size(), "plain", "black"), # ylab
                         # font.legend = c(font_size(), "plain", "black"),
                         panel.labs.font = 
                           list(face = "plain", color = "black", size = font_size(), angle = NULL), # facet label
                         ...))
  
  # return(p)
}