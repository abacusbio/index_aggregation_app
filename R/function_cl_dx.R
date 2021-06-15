# https://stackoverflow.com/questions/17924828/differences-in-heatmap-clustering-defaults-in-r-heatplot-versus-heatmap-2
# 1. heatmap.2, as default uses euclidean measure to obtain distance matrix and complete agglomeration
# method for clustering
# 2. heatmap.2 computes the distance matrix and runs clustering algorithm before scaling
# 3. heatmap.2 reorders the dendrogram based on the row and column mean values.
# Default settings (1.) can be simply changed within heatmap.2, by supplying custom distfun and 
# hclustfun arguments. However p. 2 and 3 cannot be easily addressed, without changing the source 
# code. Therefore heatplot function acts as a wrapper for heatmap.2. First, it applies necessary 
# transformation to the data, calculates distance matrix, clusters the data, and then uses heatmap.2
# functionality only to plot the heatmap with the above parameters.
# The dualScale=TRUE argument in the heatplot function, applies only row-based centering and scaling
# (description). Then, it reassigns the extremes (description) of the scaled data to the zlim values:
# z <- t(scale(t(data)))
# blow function: 
# z-score transformation is performed prior to the clustering: scale=c("row","column")
# the extreme values can be reassigned within the scaled data: zlim=c(-3,3)
# option to switch off dendrogram reordering: reorder=FALSE
# depending on the analysis, the data can be centered and scaled by row or column. 
# default parameters correspond to the ones in the heatplot function. 

#' @param x a data.frame or matrix. e.g. animal by index
#' @param bi_clust a logical value, if the heatmap should be clustered on both the column and the
#'        row
# 12s when Rowv = F (no clustering on animal), 
# 38s if bi_clust (+ col/rowsidecolors)
# 3min if show_corr
# !!! need graphics.off() on console otherwise it prints to console, not UI !!!
plotHeatmap <- function(input, output, session,
                        x, cl_obj, clusters, 
                        transpose = T, center = T, scale = T, 
                        show_corr = reactive(F), bi_clust = reactive(F)) {
cat("plotHeatmap\n")  
  if(class(cl_obj)[1] != "hclust") cl_obj <- as.hclust(cl_obj)
  if(class(x)[1]!="matrix") { x_mat <- as.matrix(x) } else {x_mat <- x}
  
  # Reorder because Rowv or Colv as dendrogram objects in heatmap.2 do not reorder
  new_order <- findObsOrder(cl_obj, k = max(clusters))
  new_cl <- as.hclust(reorder(as.dendrogram(cl_obj), new_order)) # new_cl$label==cl_obj$labels==colnames(x)
# cat(" x_mat ", class(x_mat));print(dim(x_mat))
# cat(" new_order ", class(new_order), length(new_order), "\n")
# cat(" new_cl");plot(new_cl)
cat(" start zClust", t <- Sys.time(), "\n")
  # list(dat_scaled, cor_mat = matrix/F, Rowv = dendrogram/F, Colv = dendrogram/F)
  z_mat <- zClust(x_mat, which_dim = ifelse(transpose, "column", "row"), center, scale, cl = new_cl, 
             method = cl_obj$method, show_corr(), bi_clust())
cat(" zClust finished"); print(Sys.time()-t)
# cat(" z:");print(z_mat[-1]);print(head(z_mat$Rowv$labels[z_mat$Rowv$order]))
  if(show_corr()) {
    x <- z_mat$cor_mat
  } else {
    x <- z_mat$dat_scaled
  }
  
  if(transpose) { # x is index by animal
    # ColSideColors <- as.character(rep(NA, ncol(x)))
    # RowSideColors <- as.character(clusters)
    cutree_row <- max(clusters); cutree_col = NA
  } else {        # x is animal by index
    # ColSideColors <- as.character(clusters)
    # RowSideColors <- as.character(rep(NA, nrow(x)))
    cutree_row <- NA; cutree_col = max(clusters)
  }

  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256)

cat(" starts heatmap\n"); t <- Sys.time()
  # out <- gplots::heatmap.2(x, trace = 'none', col = rev(cols), Rowv = z_mat$Rowv,
  #                          Colv = z_mat$Colv,
  #                          ColSideColors = ColSideColors, RowSideColors = RowSideColors)
#   eval(out$call)
  # return(list(x = x, col = rev(cols), Rowv = z_mat$Rowv, Colv = z_mat$Colv,
  #             ColSideColors = ColSideColors, RowSideColors = RowSideColors))
  
  out <- pheatmap::pheatmap(x, color = rev(cols),
                            cluster_rows = z_mat$Rowv, cluster_cols = z_mat$Colv,
                            cutree_rows = cutree_row, cutree_cols = cutree_col,
                            silent = T)
cat(" heatmap finished");print(Sys.time()-t)
  
  return(list(heatmap = out, data = x))
  # return(list(x = x, col = rev(cols), Rowv = cl_obj, Colv = F,
  #             ColSideColors = NA, RowSideColors = 3))
  # 
  # return(list(x = x, col = rev(cols), Rowv = z_mat$Rowv, Colv = z_mat$Colv,
  #             ColSideColors = cutree_col, RowSideColors = cutree_row))
}

#' Order the observations by their cluster sizes from the largest to the smallest
#' 
#' @param cl_obj a \code{hclust} or \link[clust]{agnes} object
findObsOrder <- function(cl_obj, k = 2) {
  
#  plot(cl_obj) # this prints to UI
 # gg <- rect.hclust(cl_obj, k = k) # this prints to UI
  gg <- split(cutree(cl_obj, k), cutree(cl_obj, k))
  gg <- sapply(gg, function(g) {
    match(names(g), cl_obj$labels)
  })
  
  cluster_order <- order(sapply(gg, length), decreasing = T) # 2 1 3
  
  if(class(cl_obj)[1]!="hclust") cl_obj <- as.hclust(cl_obj)
  
  new_order <- unlist(lapply(1:k, function(i) {
    c <- cluster_order[ i ]
    idx <- which(cl_obj$order %in% gg[[cluster_order[ i ]]])
    
    return(cl_obj$order[idx])
  }))
  return(new_order)
}

#' Generate a heatmap
#' @param x_mat a numeric matrix. e.g. animal by index.
#' @param which_dim a character value. The dimension of the observation to be scaled. Options are 
#'        "row" and "column".
#' @param bi_clust a logical value, if the heatmap should be clustered on both the column and the
#'        row
# 30s if bi_clust = F; 45s if bi_clust = T, 3s if show_corr = T
zClust <- function(x_mat, which_dim = "row", center = T, scale = T, cl,
                   method = "complete", show_corr = F, bi_clust = F) {
# cat("zClust\n which_dim:", which_dim, "\n");  
  # initialize
  if(method == "ward") method <- "ward.D2"
  hcl_row <- F; hcl_col <- F; x_cor <- F
  
  if(which_dim =="row") z <- t(scale(t(x_mat), center, scale))
  if(which_dim =="column") z <- scale(x_mat, center, scale)
  
  #if(!is.null(z)) {
  # Shrink outliers to limit values ! changed original matrix values
  if(isSymmetric(x_mat) && max(z) < 2 && min(z) < -2) { # z is a correlation matrix
    z <- pmin(pmax(z, -1), 1)
    
  } else {
    zlim <- findZlim(z)
# cat(" x_mat:", class(x_mat));cat(" z ");print(class(z));cat("zlim");print(zlim)
# cat(" pmax(z) ");print(pmax(z, zlim[1]))
    z <- pmin(pmax(z, zlim[1]), zlim[2])  
  }
  #}
  
  if(show_corr == T) { # show correlation matrix (but clustering isn't necessarily based on that)

    if(!isSymmetric(x_mat)) { # x is not a correlation matrix, make one
      
      if(which_dim == "row") { # col is obs
        x_cor <- cor(z) 
       # hcl_col <- hclust(as.dist(1-x_cor), method = method)
        hcl_col <- cl
        
        #if(bi_clust) { # may not be the best method
        #  hcl_row <- hclust(as.dist(1-cor(t(z))), method = method)
        hcl_row <- as.hclust(rev(as.dendrogram(cl)))
        #}
        
      } else { # which_dim == "col". Row is obs
        x_cor <- cor(t(z))
        # hcl_row <- hclust(as.dist(1-x_cor), method = method) # col corr
        hcl_row <- cl
        
        # if(bi_clust) { # may not be the best method
          # hcl_col <- hclust(as.dist(1-cor(z)), method = method)
        hcl_col <- as.hclust(rev(as.dendrogram(cl)))
        # }
      } # which_dim
      
    } else { # x is a correlation matrix
      x_cor <- z
      # hcl_col <- hclust(as.dist(1-x_cor), method = method)
      hcl_col <- cl
      hcl_row <- as.hclust(rev(as.dendrogram(cl)))
    }
    
  } else { # orig data
    if(isSymmetric(x_mat)) stop("zClust: x_mat should not be a correlation/covariance matrix!")
    
    if(which_dim == "row") { # col is obs
      # hcl_col <- hclust(dist(t(z)), method = method)
      hcl_col <- cl
      if(bi_clust) {
        hcl_row <- hclust(dist(z), method = method) # may not be the best method
      }
    } else {
      # hcl_row <- hclust(dist(z), method = method)
      hcl_row <- cl
      if(bi_clust) {
        hcl_col <- hclust(dist(t(z)), method = method) # may not be the best method
      }
    }
  }
  
  # # dendrogram
  # if(class(hcl_row)[ 1 ]!="logical") {
  #   hcl_row <- as.dendrogram(hcl_row)
  # }
  # 
  # if(class(hcl_col)[ 1 ]!="logical") {
  #   hcl_col <- as.dendrogram(hcl_col)
  # }
  
  return(list(dat_scaled = z, cor_mat = x_cor, Rowv = hcl_row,
              Colv = hcl_col))
}

#' Find the symmetric upper and lower boundaries for heatmap color range
#' @param z the scaled matrix
findZlim <- function(z) {
  
  m <- min(abs(range(z, na.rm = T)))
  return(c(-floor(m), floor(m)))
}


