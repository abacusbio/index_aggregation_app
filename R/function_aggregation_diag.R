
#' Plot the top n individual overlap across indexes
#' @param l a list. Each element is a data.frame of columns order, Index, plant and value (index 
#'        value)
plotTopNbar <- function(input, output, session,
                        l, agg_index_name, font_size = reactive(10)) {
cat("plotTopbar\n");cat(" agg_index_name:", agg_index_name, "\n  l name:");print(head(names(l)))
  plant_names <- l[[agg_index_name]]$plant # reference plants
# cat(" plant_names:");print(head(plant_names))
# cat(" l[[]]:\n");print(head(l[[agg_index_name]])  )
  df <- do.call(rbind, lapply(l, function(df) {
    return(data.frame(Index = df$Index[1], n =  sum(df$plant %in% plant_names),
                      percent = sum(df$plant %in% plant_names)/length(plant_names)*100))
  })) %>% arrange(desc(n))
# cat(" df:");print(head(df)  )
  x.text.angle <- ifelse(nrow(df) > 10, 90, 0)
  
  p <- ggpubr::ggbarplot(df, x = "Index", y = "percent",
                         fill = "Index", color = "white", # Set bar border colors to white
                         palette = "jco",            # jco journal color palett. see ?ggpar
                         #            sort.val = "desc",          # Sort the value in dscending order
                         # sort.by.groups = FALSE,     # Don't sort inside each group 
                         label = df$n, lab.size = 5, legend = "none", xlab = F,
                         x.text.angle = x.text.angle, font.y = c(font_size(), "plain", "black"), # y lab 
                         font.tickslab = c(font_size(), "plain", "black"))
  return(p)
}