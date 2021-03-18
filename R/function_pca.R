# 24Feb2021
# PCA: variables are Gaussian and non-independent (y is a function of x), can have linear 
# combination to form uncorrelated PC,  
#     if vars were meatured in different scale and you want to constrain the influence of variables 
#     of large variance, then scale or use correlaiton matrix.
#     if standardisation will make noise (or outlier) close to signal, then don't scale
#     if the raw number have meanings e.g. RGB 0-256, then don't center.
#     optimisation: 
#       1. XV
#       2. VALIDATION VIA MATRIX COMPLETION
#       3. Nuclear normal penalties
# PCA via SVD: SVD looks for underlying lantent variables that are non Gaussian.
#      Cons: when a variable has no correlation with others, factor analysis ignore it whereas PCA
#      takes the largest variance no matter what correlation there is
# Sparse PCA: use if p>>n to give penalty to V
#             how to determin p? For uncorrelated features, the optimal feature size is N−1 (where N
#             is sample size). As feature correlation increases, and the optimal feature size
#             becomes proportional to sqrt(N) for highly correlated features.
# kernel PCA: if PC(s) is a non-linear combination of raw variables, use kernel PCA

#' Run PCA analysis
#' 
#' @param dat a matrix or data.frame object with features in the rows and observations in the 
#'        columns. The objective is to reduce the feature dimension and cluster observations.
#' @param center logical, whether to centerize each feature. if the raw number have meanings e.g. 
#'        RGB 0-256, then don't center.
#' @param scaling logical, whether to scale all features to ~N(0, 1). if vars were meatured in 
#'        different scale and you want to constrain the influence of variables of large variance,
#'        then scale or use correlaiton matrix. if standardisation will make noise (or outlier) 
#'        close to signal, then don't scale.
#' @param obs a string vector that matches the observation names (dat colnames) to be highlighted on
#'        the PC score plot.
#'        
#'        
runPCA <- function(dat, cor_mat = NULL, center = T, scaling = F,
                   bi_plot = F, n_pc = 2, obs = NULL) {
  
  # test
  # test <- princomp(t(dat), cor = T) # centered and scaled. Otherwise using covariance and eigen decomposition
  # plotPCscore(dat, test$scores)
  # plotPCloadings(test$loadings[]) # this is different from using prcomp with center=T scale=T...
  # # pc 2, 4 probably all even number pc loadings are -1*loadings in the other method. Why? Does it
  # # matter?
  # # when explaining it, the absolute values are used, so probably doesn't matter
  
  # prcomp uses SVD, default is centred but not scaled
  pc_new <- prcomp(t(dat), center = center, scale. = scaling) # observation x feature (to reduce) # x observation
  pc_scores <- pc_new$x # ncol = n orig features
  
  # variance explained
  # screeplot(pc_new)
  varex <- 100*pc_new$sdev^2/sum(pc_new$sdev^2)
  p_pc_var <- plotPCvar(varex)
  
  # cumulative variance explained
  cvarex <- cumsum(varex) # cumsum(varex[1:ncol(dat)])
  p_pc_cumvar <- plotPCvar(cvarex, cumulative = T)
  
  # if(bi_plot) {     # default R plots with princomp/prcomp
  #   p_biplot <- biplot(pc_new, cex = -.7) # both feature and observation # can't save to an object
  # } else {
  #   p_biplot <- NULL
  # }
  
  # scatter plots - patterns among observations. Not as good as codes on line 228 if more than 2 pcs
  p_score_scatter <- plotPCscore(dat, pc_scores, n_pc = n_pc, obs = obs) # single ggplot object
  # or a list. can use gridExtra::grid.arrange(tempVar$plot)
  
  # PC loadings - variables that contribute to these patterns
  pc_loading <- data.frame(pc_new$rotation, check.names = T)  # 64x64
  p_loading <- plotPCloadings(pc_new$rotation, n_pc = n_pc) # takes long time if too many features
  
  return(pc_scores = pc_scores, pc_var = varex, pc_cumvar = cvarex, pc_loading = pc_loading,
         plot_pc_var = p_pc_var, plot_pc_cumvar = p_pc_cumvar, plot_biplot = p_biplot,
         plot_scores = p_score_scatter, plot_loading = p_loading)
}

#' Plot PC variances
#' @param varex a numeric vector of PC variances or cumulative pc variance
plotPCvar <- function(varex, cumulative = F, sparse = F) {
  
  if(sparse) {
    xlab <- ifelse(cumulative, "Number of sparse components", "Sparse principle Components")
  } else {
    xlab <- ifelse(cumulative, "Number of components", "Principle Components")
  }
  
  return(ggplot(data.frame(varex = varex, n = seq(varex)), aes(n, varex)) +
    geom_point() +
    geom_line() +
    labs(title = "% Variance Explained", x = xlab, y = "%") +
    theme_minimal())
}

#' Plot a scatter plot of any 2 principle component scores
#' 
#' @description usually the PC scores are for observations, and the scatter plot can show the 
#'              relationship pattern among observations.
#'              
#' @param dat a matrix or data.frame object with features in the rows and observations in the 
#'        columns           
#' @param pc_scores a matrix from prcomp output value "x" object
#' @param n_pc an integer, the number of largest PC to plot
#' @param obs a string, the observation name(s) (row names) of the pc_score to highlight in the plot
#' @param group a vector, the grouping of the observation name. The vector's names are identical to 
#'        the observation names. e.g. cutree output value
#' 
plotPCscore <- function(dat, pc_scores, n_pc = 2, obs = NULL, group = NULL) {
  
  if(sum(grepl("data.frame", class(pc_scores)))==0) {
    df <- data.frame(pc_scores) # duplicated row name will concatenate with .1, .2 ...
  } else {
    df <- pc_scores
  }
  
  combinations <- combn(1:n_pc, 2) # n_pc is the number of pcs
  p_list <- lapply(1:ncol(combinations), function(i) {
    combination <- combinations[,i,drop = T]
    
    p <- ggplot(df, aes_string(x = names(df)[combination[1]], y = names(df)[combination[2]])) +
      geom_point() 
    
    if(!is.null(obs)) {
      ind = grep(obs,rownames(t(dat))) # match() will be exact match
      tmp <- dplyr::slice(df, ind)
      
      p <- p + 
        geom_text(data = tmp, aes(color = "red"), label = rownames(df)[ind]) + 
        theme_minimal() + theme(legend.position = "none")
      
    } else { # look at a particular observation
      p <- p + 
        geom_text(label = rownames(df)) +
        theme_minimal()

      if(!is.null(group)) { # color the plots by grouping
        if(sum(is.null(match(names(group), rownames(pc_scores))))==0) { # skip if names do not match
          
          group <- data.frame(group = as.matrix(group))
          group$obs <- rownames(group) 
          group$group <- as.factor(group$group)
          group <- dplyr::distinct(group)
          
          df$obs <- rownames(df)
          df <- dplyr::left_join(df, group, by = "obs")
          
          p <- ggplot(df, aes_string(x = names(df)[combination[1]], 
                                     y = names(df)[combination[2]])) +
            geom_point(color = "grey") +
            ggrepel::geom_text_repel(color = df$group, label = df$obs) +
            theme_minimal()
        }
      }
    }
  
    return(p)
  })
  
  return(p_list)
}

#' Plot PC loadings in bar charts
#' @param rotation a matrix of features by rotated PCs, e.g. "rotation" object of prcomp output
#' @param n_pc an integer, the number of largest PC to plot
#' 
plotPCloadings <- function(rotation, n_pc = 2, title = "PC loadings") {
  
  tmp <- data.frame(rotation) # duplicated row names will be catenated with .1, .2 ...
  
  df_loading <- do.call(rbind, lapply(1:n_pc, function(i) {
    out <- data.frame(observation = rownames(tmp), loading = rotation[,i], 
                      class =colnames(rotation)[ i ])
    out$col <- ifelse(out$loading > 0, "1", "0") # pos and neg values in different colors
    return(out)
  }))
  
  p <- ggplot(df_loading, aes(observation, loading, fill = col)) +
    geom_col(color = ifelse(nrow(df_loading) < 20, "black", "white")) +
    facet_grid(class~.) +
    labs(title = title) + 
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(), axis.text.x = element_text(angle = 90))
  
  if(nrow(df_loading) < 20) {
    p <- p + 
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  }
  
  return(p) # print(p) takes long time if too many pcs
}

#' Run sparse PCA using library PMA
#' 
#' @description use if p>>n to give penalty to V. No good if otherwise.
#'           how to determin p? For uncorrelated features, the optimal feature size is N−1 (where N
#'           is the sample size). As feature correlation increases, and the optimal feature size
#'           becomes proportional to sqrt(N) for highly correlated features.
#' @param sumabsv, numeric, the sparse paramter in PMA::SPC. If not provided, can derive in this
#'        function.
#' @param k, numeric, as K, the number of factors in PMA::SPC. Default is 3.
#' @param choice string, the preference when choosing \code{sumabsv}. Default is "sparsest".
#'        options also include "balanced" and "lowest cross validation error".
#' @param center logical, whether to centerize each feature. if the raw number have meanings e.g. 
#'        RGB 0-256, then don't center.
#' @param scaling logical, whether to scale all features to ~N(0, 1). if vars were meatured in 
#'        different scale and you want to constrain the influence of variables of large variance,
#'        then scale or use correlaiton matrix. if standardisation will make noise (or outlier) 
#'        close to signal, then don't scale.
#' @param obs a string vector that matches the observation names (dat colnames) to be highlighted on
#'        the PC score plot.
#'         
runSparsePCA <- function(dat, sumabsv = NULL, k = 3, choice = "sparsest",
                         center = T, scaling = F,
                         bi_plot = F, n_pc = 2, obs = NULL) {
  
  if(is.null(sumabsv)) {
cat("find the best sumabsv\n")
    tmp <- find_sumabsv(dat, k, choice, center, scaling)
    sumabsv <- tmp$sumabsv
    warning_text <- tmp$warning_text
  } # if sumabsv isn't provided
  
  # sumabsv: how sparse [1, sqrt(col(cdat))]; K: n factors to return in PMD
  spc <- PMA::SPC(scale(t(ncidat), center = center, scale = scaling), 
                  sumabsv = sumabsv, K = k, trace = F) 
  
  # variance explained
  varex <- sort(spc$prop.var.explained, decreasing = T) # similar to PCA, in 100% scale already, sort ascending
  p_pc_var <- plotPCvar(varex, sparse = T)
  
  # cumulative variance explained
  cvarex <- cumsum(varex) # cumsum(varex[1:ncol(dat)])
  p_pc_cumvar <- plotPCvar(cvarex, cumulative = T, sparse = T)
  
  # scatterplots of Sparse PCs. Not as good as line 228 if more than 2 pcs
  spc_score <- spc$u[,order(ncol(spc$u):1)] # matrix
  colnames(spc_score) <- paste0("sparsePC_", 1:ncol(spc_score))
  rownames(spc_score) <- colnames(dat) # observation
  
  p_score_scatter <- plotPCscore(dat, spc_score, n_pc = k, obs = NULL, group = NULL)
  
  # PC loadings
  spcL <- data.frame(spc$v[, order(ncol(spc$v):1)], check.names = T) # 3 cols because K=3. Some features are removed (0)
  rownames(spcL) <- colnames(t(dat)) # features
  names(spcL) <- paste0("sparse_loarding_", 1:ncol(spcL))
  
  p_loading <- plotPCloadings(spcL, n_pc = n_pc, title = "Sparse PC loadings")
  
  return(pc_scores = spc_scores, pc_var = varex, pc_cumvar = cvarex, pc_loading = spcL,
         plot_pc_var = p_pc_var, plot_pc_cumvar = p_pc_cumvar, plot_biplot = p_biplot,
         plot_scores = p_score_scatter, plot_loading = p_loading)
}

#' Find the desired sumabsv to run sparse PCA using PMA::spc
#' 
#' @param sumabsv, numeric, the sparse paramter in PMA::SPC. If not provided, can derive in this
#'        function.
#' @param k, numeric, as K, the number of factors in PMA::SPC. Default is 3.
#' @param choice string, the preference when choosing \code{sumabsv}. Default is "sparsest".
#'        options also include "balanced" and "lowest cross validation error".
#' @param center logical, whether to centerize each feature. if the raw number have meanings e.g. 
#'        RGB 0-256, then don't center.
#' @param scaling logical, whether to scale all features to ~N(0, 1). if vars were meatured in 
#'        different scale and you want to constrain the influence of variables of large variance,
#'        then scale or use correlaiton matrix. if standardisation will make noise (or outlier) 
#'        close to signal, then don't scale.
#' @details Test sumabsv from 1 to sqrt(n features) until criterion reached. e.g. least number 
#'          of all 0 rows. each row has at least k-1 0s, but not too many all-0 rows (K 0s). 
#'          Starting from 1, most rows are all 0s. test until there are rows that are all non-0s...

find_sumabsv <- function(dat, k = 3, choice = "sparsest", center = T, scaling = F) {
  
  tmp <- lapply(1:floor(sqrt(nrow(dat))), function(i) { # 1 to sqrt(n features)
    spc <- PMA::SPC(scale(t(ncidat), center = center, scale = scaling), 
                    sumabsv = i, K = k, trace = F)
    all_0 <- apply(spc$v, 1, function(x) x==0) %>% apply(2, sum)
    
    return(c(sum(all_0==3)/length(all_0), sum(all_0==0)/length(all_0),
             sum(all_0==3), sum(all_0==0)))
  })
  tmp <- do.call(rbind.data.frame, tmp)
  names(tmp) <- c("perc_abandon_feature", "perc_dense_feature", "n_abandon_feature",
                  "n_dense_feature")
  
  # plot(test$perc_abandon_feature, test$perc_dense_feature) # sanity check
  # plot(test$perc_abandon_feature)
  # points(test$perc_dense_feature, col = "green")
  # abline(v = 46)
  
  # predefined choice of sumabsv 
  if(choice == "sparsed") {
    sumabsv <- min(which(tmp$perc_dense_feature > 0))-1 # least dense point
    
  } else if(choice == "balanced") {
    sumabsv <- min(which(tmp$perc_dense_feature > tmp$perc_abandon_feature))-1 # balance point
    
  } else if(choice == "lowest cross Validation error") {
    spc <- PMA::SPC.cv(scale(t(dat), center = center, scale = scaling),
                         sumabsv= c(1, floor(sqrt(nrow(dat)))), niter = 100, trace = F)
    sumabsv <- spc$bestsumabsv # lowest CV error, but in ncidata case it's least sparse...not very useful
  }
  
  warning_text <- paste0("You chose the sparsity parameter based on '", choice, "' criterion,
                        obtained sumabsv value of ", sumabsv, ". This dropped ",
                         tmp$n_abandon_feature[sumabsv], "features (",
                         tmp$perc_abandon_feature[sumabsv], "% of total number of features),
                       of which ", tmp$n_dense_feature[sumabsv], " are dense (",
                         tmp$perc_dense_feature[sumabsv], "%.")
  return(list(sumabsv = sumabsv, warning_tesx = warning_text))
}