#' @param df a data.frame
plotGroupedBar <- function(input, output, session,
                           df, x, y, group, ylab = NULL, font_size = reactive(12),
                           baseline = F, ...) {
# cat("plotGroupedBar\n df:\n");print(df);print(unique(df[,group]));print(length(unique(df[,group])));print(class(unique(df[,group])))
  x.text.angle <- ifelse(length(unique(df[[x]]))<10, 0, 90)
  
  if(baseline) {
    palettes <- c("#808080", # avg_index
                  ggpubr::get_palette("npg", length(unique(df[[group]]))))
  } else { 
    palettes <- ggpubr::get_palette("npg", length(unique(df[[group]])))
  } 
  
  return(
  ggpubr::ggbarplot(df, x = x, y = y, color = "white", fill = group, 
                    palette = palettes,
                    position = position_dodge(0.8), lable = T,
                    xlab = F, ylab = ylab, 
                    font.x = c(font_size(), "plain", "black"), # xlab
                    font.y = c(font_size(), "plain", "black"), # y lab
                    font.legend = c(font_size(), "plain", "black"),
                    font.tickslab = c(font_size()+2, "plain", "black"),
                    x.text.angle = x.text.angle,
                    ...))
}