#' Run cluster analysis using either raw data or its correlation matrix
#' 
#' @description already got k and best_method, produce cluster groups directly
#' 
runFinalCluster <- function(dat, cor_mat = F, cluster_object = NULL,
                              scale = T, center = T, k = 2, best_method = "complete") {
  
  if(class(cluster_object)[1] == "NULL") { # use raw data
    dat <- t(scale(t(dat), center, scale))
    
    # use agnes
    # this cluster the col of dat (observation)
    if(cor_mat) {
      cluster_object <- agnes(as.dist(1-cor_mat), diss = T, method = best_method)
    } else {
      cluster_object <- agnes(t(dat), method = best_method) # same as agnes(daisy(t(dat)), diss = T)
    }
  } else if(!class(cluster_object)[1] %in% c("agnes", "hclust")) { # error
    
    stop("runFinalCluster, error: class(cluster_object)", class(cluster_object)[1])
  }
  
  clusters <- cutree(cluster_object, k = k)
  if(is.null(names(clusters))) names(clusters) <- colnames(dat)
  # h3 <- heatmap.2(cor_mat, symm = T, col = gcol2, trace = "none",
  #                 hclustfun = function(x) hclust(x, "ward.D2"),
  #                 distfun = function(x) as.dist(1 - x)) # 1 - abs(x) ?? # 
  return(list(cluster_obj = cluster_object, clusters = clusters))
}


# Run cluster analysis using either raw data or its correlation matrix
#'
#'@param dat a numeric matrix or data.frame object with features in the rows and observations in  
#'       the columns. The objective is to reduce the feature dimension and cluster observations
#'@param cor_mat logical, if T then dat is a correlation matrix instead. Default is F
#'@param stand logical, whether to standardise dat. The arg will be ignored when \code{cor_mat} is 
#'       \code{TRUE}
runCluster <- function(dat, cor_mat = F, scale = T, center = T) {
  
  dat <- t(scale(t(dat), center, scale))
  
  # use agnes to choose the best agglomeration
  # methods to assess
  m <- c( "average", "single", "complete", "ward", "weighted") # ward = ward.D2 in hclust
  names(m) <- m
  
  # function to compute agglomerative coefficient. Closer to 1 the better.
  
  if(cor_mat) {
    ac <- function(x) agnes(as.dist(1-cor_mat), diss = T, method = x)$ac
  } else {
    ac <- function(x) agnes(t(dat), method = x)$ac # same as agnes(daisy(t(dat)), diss = T)
  }
  
  af <- purrr::map(m, ac)
  best_method <- names(which.min(1-abs(unlist(af))))
  
  cl <- agnes(t(dat), method = best_method) # this cluster the col of dat (observation)
  
  # h3 <- heatmap.2(cor_mat, symm = T, col = gcol2, trace = "none",
  #                 hclustfun = function(x) hclust(x, "ward.D2"),
  #                 distfun = function(x) as.dist(1 - x)) # 1 - abs(x) ?? # 
  return(list(cluster_obj = cl, best_method = best_method, agg_coefs = af))
}

#' Cut the cluster into k clusters or by optimal algorithms
#'@param dat a numeric matrix or data.frame object with features in the rows and observations in  
#'       the columns. The objective is to reduce the feature dimension and cluster observations
#'@param cl an object of class \code{agnes} or \code{hclust} out of corresponding 
#'       functions
#'@param hc_method, the agglomeration method. Options are "ward" (agnes), "ward.D2" (hcluster),
#'       "average", "single", "complete"...for corresponding cluster function
#'@param k an integer, the number of clusters to cut. Default is 0 and the optimal cut will be 
#'       calculated by 
#'@detail choose the best cutoff: \url{https://uc-r.github.io/hc_clustering}
findOptimalCut <- function(dat, cl, hc_method = "ward"
                           #input, output, session
                           ) {
cat("findOptimalCut\n cl:") # ;print(cl);
#cat(" ### BEFORE ###\n  cl$height:");print(cl$height);cat(" diff height:");print(diff(cl$height))
  cl_orig <- cl
  if(class(cl)[1]!="hclust") cl <- as.hclust(cl) # agnes height not sorted
  
  # largest height change cutoff
  cl$height <- round(cl$height, 6)
  idx <- which.max(diff(cl$height))-1
  if(idx==0) idx <- which(diff(cl$height) == sort(diff(cl$height), decreasing = T)[2]) -1
  h <- cl$height[idx]
#cat(" ### AFTER ###\n cl$height:");print(cl$height);cat(" diff height:");print(diff(cl$height))
#cat(" h:", h, "\n");plot.new();plot(cl);rect.hclust(cl, h = h)
  clusters <- cutree(cl, h = h)
  k_h <- max(clusters)
  
  # largest total sum of squares change cutoff
  p_tss <- factoextra::fviz_nbclust(t(dat), factoextra::hcut, method = "wss",
                                    k.max = nrow(dat) - 1,
                                    hc_func = class(cl_orig)[1], hc_method = hc_method) # hcut args
  diff_tss <- diff(p_tss$data$y)
  k_tss <- which.max(abs(diff(p_tss$data$y))/p_tss$data$y[1]) + 1
  p_tss <- p_tss + 
    ggplot2::geom_vline(xintercept = k_tss, linetype = "dashed", color = "steelblue") +
    ggplot2::labs(subtitle = paste0("Agglomerative method: ", hc_method))
  
  # largest silouette cutoff
  p_sil <- factoextra::fviz_nbclust(t(dat), factoextra::hcut, method = "silhouette",
                                    k.max = nrow(dat) - 1,
                                    hc_func = class(cl_orig)[1], hc_method = hc_method) + # hcut args
    ggplot2::labs(subtitle = paste0("Agglomerative method: ", hc_method))
  k_sil <- which.max(p_sil$data$y)
  
  return(list(k_h = k_h, k_tss = k_tss, k_sil = k_sil,
              h = h, p_tss = p_tss, p_sil = p_sil)) # print(p_tss/psil to get the plot)
}

#' Get heatmap and clusters of rows and cols from PCA results
#' 
#' @description This is the hcluster out of PCA outputs. Best used for bi-cluster
#' @param dat a numeric matrix or data.frame object with features in the rows and observations in  
#'        the columns. The objective is to reduce the feature dimension and cluster observations.
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
runHeatmap <- function(dat, pc_loading, center = T, scaling = F, # cor_mat = NULL, 
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
findFeatureOrder <- function(pc_loading, feature_index) {
  
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
runBiCluster <- function(h2, what_to_cluster = "rowDendrogram", k = 3) {
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

