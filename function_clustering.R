#' 
#' @description This is the hcluster out of PCA outputs
#' @param pc_loading a matrix of PC loadings of dimension n features by n observations, e.g. 
#'        "rotation" object of \code{prcomp} output
#' @param feature_index an integer or a vector, the index of the new feature(s) (PC loadings) 
#'        column to use for clustering
#' @param subset_top_feature integer, the number of the top new features (PC loadings) column to use
#'        for the heatmap. This is equivalent to choose the largest PCs.
#' @param cluster, a logical value, whether to operate hierachical clustering analysis
#' @param what_to_cluster a string, the hcluster object to retrieve from \code{gplots::heatmap.2}
#'        outputs. Options are "rowDendrogram" and "colDendrogram". This works when \code{cluster}
#'        is TRUE       
find_bi_cluster <- function(dat, pc_loading, center = T, scaling = F, n_pc, feature_index = 1, 
                         subset_top_feature = 3, cluster = F,
                         what_to_cluster = "rowDendrogram", k) {
  
  # Center
  X <- t(scale(t(dat), center = center, scale = scaling)) # matrix, same dim as dat, center dat rows
  
  # PC loadings - visualize data by limiting to top genes in magnitude in the PC loadings (melanoma)
  # For bi-clustering
  gcol2 <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(
    min(length(unique(as.vector(X))), 256)) # if only pos, "Oranges". If only neg, "Blues"
  
  ## cluster based on the chosen PC/loading and top original features
  ord <- find_feature_order(pc_loading, feature_index) # feature ordering
  x <- as.matrix(X[ord[1:subset_top_feature],]) # take 250 largest PCs, look at the melanoma pattern
  h2 <- heatmap.2(x, scale = "none", col = gcol2, trace = "none")
  
  if(cluster) {
    # heatmap.2 by default uses euclidean distance and complete agglomeration method for clustering
    # colDendrogram: observation clusters. rowDendrogram: original feature clusters.
    # or col.clusters2 <- hclust(dist(t(x))) for colDendrogram
    col.clusters2 <- as.hclust(h2[[what_to_cluster]] ) #
    cutree(col.clusters2, k = 3) # break into k=3 clusters
    # to choose the best cutoff: https://uc-r.github.io/hc_clustering
  } else {
    clusters <- NULL
  }
  
  return(list())
}

#' Find the order of the original features according to the absolute value of the selected PC 
#' loading
#' 
#' @description if selected multiple features, return the average order across all features
#' 
find_feature_order <- function(pc_loading, feature_index) {
  
  if(length(feature_index)==1) {
    ord <- order(abs(pc_loading[, feature_index]), decreasing = T) # feature ordering
  } else {
    
    #df <- data.frame(pc_loading, index = 1:nrow(pc_loading))
    
    ords <- apply(abs(pc_loading[,featuer_index]), 2, order, decreasing = T) # n orig feature x n pc
    
    seqs <- lapply(1:ncol(ords), function( i ) {
     ord <- ords[, i]
      idx <- 1:nrow(pc_loading)
      
      seqs <- vector("integer", nrow(pc_loading)) # create a rank for orignial feature
      for(i in 1:length(seqs)) {
        seqs[ ord[ i ] ] <- idx[ i ]
      }
      
      return(seqs) # loading, ord
    })
    seqs <- do.call(cbind, seqs)
    
    ord <- order(apply(seqs, 2, mean)) # average rank of original features across n PCs
  }
  
  return(ord)
}

find_cluster # use correlation matrix or original data  
