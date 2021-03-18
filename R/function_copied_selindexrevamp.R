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
  dendr <- ggdendro::dendro_data(cl, type="rectangle") 
  text.df<-merge(ggdendro::label(dendr),clust.gr,by.x="label",by.y="row.names")
  text.df <- merge(text.df, clcut1, by = "label")
  
  pDendro <- ggplot() + 
    geom_segment(data = ggdendro::segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_label(data=text.df, aes(x=x, y=y, label=label, hjust=0, fill=group), color = "black") +
    scale_fill_brewer(palette = "Set3")
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