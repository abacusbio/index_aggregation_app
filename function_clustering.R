#' Get heatmap and clusters of rows and cols from PCA results
#' 
#' @description This is the hcluster out of PCA outputs. Best used for bi-cluster
#' @param pc_loading a matrix of PC loadings of dimension n features by n observations, e.g. 
#'        "rotation" object of \code{prcomp} output
#' @param n_pc an integer indicating the top number of PC to look at        
#' @param feature_index an integer value, the index of the new feature (PC loading) 
#'        column to use for clustering. This will overwrite \code{n_pc}.
#' @param subset_top_feature integer, the number of the top new features (PC loadings) column to use
#'        for the heatmap. This is equivalent to choose the largest PCs. If chose too many, can take
#'        long time to run
#' 
#' @return a \code{\link[gplots]{heatmap.2}} output object
run_heatmap <- function(dat, pc_loading, center = T, scaling = F, # cor_mat = NULL, 
                        n_pc, feature_index = 0, subset_top_feature = 3) {
  hclustfun = function(x) hclust(x,method = 'centroid')
  
  # Center features (dat rows)
  X <- t(scale(t(dat), center = center, scale = scaling)) # matrix, same dim as dat
  
  # PC loadings - visualize data by limiting to top genes in magnitude in the PC loadings (melanoma)
  # For bi-clustering
  gcol2 <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(
    min(length(unique(as.vector(X))), 256)) # if only pos, "Oranges". If only neg, "Blues"
  
  # Heatmap based on the chosen PC/loading and top original features
  ## feature ordering
  if(feature_index == 0) { 
    ord <- find_feature_order(pc_loading, n_pc) 
  } else {
    ord <- find_feature_order(pc_loading, feature_index) 
  }
  # take 250 largest PCs, look at the melanoma pattern
  x <- as.matrix(X[ord[1:subset_top_feature],])
  
  # if(is.null(cor_mat)) {
    # heatmap.2 by default uses euclidean distance and complete agglomeration method for clustering
    h2 <- heatmap.2(x, scale = "none", col = gcol2, trace = "none")
    # !!! to plot again, should do eval(h2$call). How to do that in shiny app? renderPlot({eval(h2$call)})?
  # } else {

    # h3 <- heatmap.2(cor_mat, symm = T, col = gcol2, trace = "none",
                    # hclustfun = function(x) hclust(x, "ward.D2"),
                    # distfun = function(x) as.dist(1 - x)) # 1 - abs(x) ??
  # }
  
  return(h2)
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

#' Run hcluster using heatmap.2 objects
#' 
#' @param h2 the \code{\link[gplots]{heatmap.2}} output object
#' @param what_to_cluster a string, the hcluster object to retrieve from \code{gplots::heatmap.2}
#'        outputs. Options are "rowDendrogram" and "colDendrogram". This works when \code{cluster}
#'        is TRUE       
#' @param k an integer indicating the number of clusters to cut. If \code{NULL} will search for the
#'        optimal k
#' 
run_bi_cluster <- function(h2, what_to_cluster = "rowDendrogram", k = 3) {
  # heatmap.2 by default uses euclidean distance and complete agglomeration method for clustering
  # colDendrogram: observation clusters. rowDendrogram: original feature clusters.
  # or cl <- hclust(dist(t(x))) for colDendrogram
  cl <- as.hclust(h2[[what_to_cluster]] )
  
  if(is.null(k)) { # search a k
    # choose the best cutoff: \url{https://uc-r.github.io/hc_clusterin}
    idx <- which.max(diff(cl$height))-1
    if(idx==0) idx <- which(diff(cl$height) == sort(diff(cl$height), decreasing = T)[2]) -1
    
    clusters <- cutree(cl, h = cl$height[idx])
  } else {
    
    clusters <- cutree(cl, k = k) # break into k clusters  
  } # if k isn't provided
  
  return(clusters)
}

# use correlation matrix or original data, without heatmap.2
find_cluster <- function(dat, 
                         #cor_mat, 
                         ) {
 # use agnes to choose the best agglomeration
  # methods to assess
  m <- c( "average", "single", "complete", "ward", "weighted")
  names(m) <- m
  
  # function to compute agglomerative coefficient. Closer to 1 the better.
  ac <- function(x) {
    agnes(t(dat), method = x)$ac
  }
  
  af <- purrr::map(m, ac)
  best_method <- names(which.min(1-abs(unlist(af))))
  
  cl <- agnes(t(dat), method = best_method) # this cluster the col of dat (observation)
  
  # h3 <- heatmap.2(cor_mat, symm = T, col = gcol2, trace = "none",
  #                 hclustfun = function(x) hclust(x, "ward.D2"),
  #                 distfun = function(x) as.dist(1 - x)) # 1 - abs(x) ?? # 
  
}
