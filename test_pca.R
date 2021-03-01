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
# ICA: use if variables are independent and non-Gaussian, e.g. cocktail party noise reduction.
#     cons: non unique solution, depending on k; factors are unordered and non orthogonal; need 
#     permutation to get the right solution
# NMF: use if variables are non-negative e.g. RGB, texts. 
#     Can add an intercept to the raw data to force non-negative before doing NMF
#     factors are archetypes
#     cons: non unique non global solution, depending on k; initialisation matters (run several 
#           times and take the best); factors are unordered and non orthogonal
# multidimensiontal scaling (MDS): can use to visualise distance in a lower dimention space

#library(nsprcomp)
library(ggplot2)
library(PMA)
library(gplots)

require("ISLR")
ncidat = t(NCI60$data) # gene x cancer, all columns centered around 0, range [-6,6]
colnames(ncidat) = NCI60$labs

dim(ncidat) #6830*64 gene*cancer type of a person, where genes are features
unique(colnames(ncidat))

# prcomp uses SVD, default is centred but not scaled
pc_new <- prcomp(t(ncidat), scale. = F) # observation x feature (to reduce) # x observation
pc_scores <- data.frame(pc_new$x, check.rows = F) # check.rows = T gives .1 to duplicated row names
#default R plots with princomp
biplot(pc_new, cex = -.7) # both feature and observation

#scatter plots - patterns among observations. Not as good as codes on line 228 if more than 2 pcs
i = 1; j = 2;
ggplot(pc_scores, aes_string(x=paste0("PC", i), y=paste0("PC", j))) +
  geom_point() + 
  geom_text(label=rownames(pc_scores)) +
  theme_minimal()

#look at a particular college
ind = grep("CNS",rownames(t(ncidat))) # match() will be exact match
# text(pc_new$x[ind,i],pc_new$x[ind,j],rownames(ncidat)[ind],cex=.7,col=2)

tmp <- dplyr::slice(data.frame(pc_scores), ind)
ggplot(data.frame(pc_scores), aes_string(x=paste0("PC", i), y=paste0("PC", j))) +
  geom_point() + 
#  geom_text(label=rownames(pc_scores)) +
  geom_text(data = tmp, aes(color = "red"), label = rownames(pc_scores)[ind]) + 
  theme_minimal() + theme(legend.position = "none")


#loadings - variables that contribute to these patterns
# par(mfrow=c(4,1)) # cols are PCs, rows are ys
# barplot(pc_new$rotation[,1], cex.name=.6, main = "PC 1 Loadings") # 64x64
# barplot(pc_new$rotation[,2], cex.name=.6, main = "PC 2 Loadings")

tmp <- data.frame(pc_new$rotation, check.rows = T)  # 64x64
df_loading <- do.call(rbind, lapply(1:4, function(i) {
  out <- data.frame(observation = rownames(tmp), loading = pc_new$rotation[,i], 
             class =colnames(pc_new$rotation)[ i ])
  out$col <- ifelse(out$loading > 0, "1", "0") # pos and neg values in different colors
  return(out)
}))
p <- ggplot(df_loading, aes(observation, loading, fill = col)) +
  geom_col(color = ifelse(nrow(df_loading) < 20, "black", "white")) +
  facet_grid(class~.) +
  labs(title = "PC loadings") + 
  theme_minimal() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), axis.text.x = element_text(angle = 90))

if(nrow(df_loading) < 20) {
  p <- p + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
}

print(p) # takes long time if too many pcs

# variance explained
# screeplot(pc_new)

varex = 100*pc_new$sdev^2/sum(pc_new$sdev^2)
plot(varex,type="b",lty = 1, 
     title = "% Variance Explained",xlab="Principle Component", ylab = "%")

#cumulative variance explained
cvarex <- cumsum(varex[1:ncol(ncidat)])
plot(cvarex,type="b", lty = 1,
     title = "Cumulative Variance Explained",xlab = "Component", ylab = "%")

######
# sparse PCA. Best for when p>>n, to give penalty to V
library(PMA) #this package requires impute which is not OK for R3.3.1

k <- 3
spc <- SPC(scale(t(ncidat)),sumabsv=2,K=k) # sumabsv: how sparse [1, sqrt(col(cdat))]; K: n factors to return in PMD

# test sumabsv from 1 to sqrt(ncol(cdat)) until each row has at least k-1 0s, but not too many all-0
# rows (K 0s). e.g. least number of all 0 rows
# starting from 1, most rows are all 0s. test until there are rows that are all non-0s. Then use i-1
i <- 0
sumabsv <- NULL
all_0 <- rep(3, nrow(spc$v))
while(!any(all_0==0)) {
  i <- i + 1
  spc <- SPC(scale(t(ncidat)),sumabsv=i,K=k)
  all_0 <- apply(spc$v, 1, function(x) x==0) %>% apply(2, sum)
}
sumabsv <- i-1
spc <- SPC(scale(t(ncidat)),sumabsv= sumabsv,K=k)

spcL = data.frame(spc$v, check.rows = F) # 3 cols because K=3. Books and Personal are removed (0)
rownames(spcL) = names(t(ncidat)) # features
names(spcL) <- paste0("sparse_loarding_", 1:ncol(spcL))

#scatterplots of Sparse PCs. Not as good as line 228 if more than 2 pcs
spc_score <- data.frame(spc$u)
names(spc_score) <- paste0("sparsePC_", 1:ncol(spc_score))
rownames(spc_score) <- rownames(cdat)
par(mfrow=c(1,1))

i = 1; j = 2;
# plot(spc$u[,i],spc$u[,j],pch=16,cex=.2)
# text(spc$u[,i],spc$u[,j],rownames(cdat),cex=.6)

ggplot(spc_score, aes_string(x=paste0("sparsePC_", i), y=paste0("sparsePC_", j))) +
  geom_point() + 
  geom_text(label=rownames(spc_score)) +
  theme_minimal()

#loadings 
par(mfrow=c(2,1))
barplot(spc$v[,1],names=names(ncidat),cex.names=.6,main="SPC 1 Loadings") # different from PCA
barplot(spc$v[,2],names=names(ncidat),cex.names=.6,main="SPC 2 Loadings") # like PC loading comp.1

df_loading <- do.call(rbind, lapply(1:k, function(i) {
  out <- data.frame(observation = rownames(spcL), loading = spcL[,i], class =colnames(spcL)[ i ])
  out$col <- ifelse(out$loading > 0, "1", "0") # pos and neg values in different colors
  return(out)
}))

# more plot argument see above
ggplot(df_loading, aes(observation, loading, fill = col)) + # takes long time if too many features
  geom_col() +
  facet_grid(class~.) +
  labs(title = "Sparse PC loadings") + 
  theme_minimal() + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_text(angle = 90))


#variance explained
spc$prop.var.explained # similar to PCA, in 100% scale already, sort ascending

varex = 100*pc_new$sdev^2/sum(pc_new$sdev^2)
plot(varex,type="b",lty = 1, 
     title = "% Variance Explained",xlab="Principle Component", ylab = "%")

#cumulative variance explained
cvarex <- cumsum(varex[1:ncol(ncidat)])
plot(cvarex,type="b", lty = 1,
     title = "Cumulative Variance Explained",xlab = "Component", ylab = "%")


##########################################################
# Dataset 2 - NCI Microarray Data
# Understand PCA and Sparse PCA
# PCA solution via the SVD
###########################################################

require("ISLR")

ncidat = t(NCI60$data) # gene x cancer, all columns centered around 0, range [-6,6]
colnames(ncidat) = NCI60$labs

dim(ncidat) #6830*64 gene*cancer type of a person, where genes are features
unique(colnames(ncidat))

#PCA - take SVD to get solution
#center genes, but don't scale， because SVD on centered x is equivalent to obtaining a covariance
# matrix for PCA. https://stats.stackexchange.com/questions/189822/how-does-centering-make-a-difference-in-pca-for-svd-and-eigen-decomposition
X = t(scale(t(ncidat),center=TRUE,scale=FALSE)) # 6830 * 64, a[,1]-mean(a[,1]) for all cols
sv = svd(t(X)); # t(X) is 64x6830, obs x feature
U = sv$u # singular vector, left, 64x64, unscaled PCs
# S = sqrt(N)U is the underlying latent variables (factors). This is INDEPENDENT & uncorrelated Gaussian vars.
# And is a linear combination of original vars
D = sv$d # singular values, the sd of variance, length==64 (diagonal)
V = sv$v # singular vector, right, 6830x64, also == PC loadings (cols), not sorted, uncorrelated
Z = t(X)%*%V; # PC, 64x64, or Z <- U%*%diag(D, 64, 64) 

# sanity check if SVD is the same as prcomp. Should all be 0s
i <- sample(1:(nrow(V)-6), 1)
j <- sample(1:(ncol(V)-6), 1)
round(Z-pc_scores, 3)[j:(j+6),j:(j+6)]
round(V-pc_new$rotation, 3)[i:(i+6),j:(j+6)]

# 23Feb2021 diagnosis X vs ncidat to see how centering works
summary(X-ncidat)
par(mfrow=c(2,1))
boxplot(ncidat, title = "raw data")
boxplot(X, title = "centered") # hard to see diff
par(mfrow=c(1,1))
boxplot(X-ncidat, title = "diff") # range roughly -1~1

# 24Feb2021
par(mfrow=c(3,1))
plot(pc_new$rotation[,1], pc_new$rotation[,2])
plot(U[,1], U[,2])
plot(Z[,1], Z[,2])

# PC scatterplots for bi-clustering
cols = as.numeric(as.factor(colnames(ncidat))) # color for observations
K = 3
pclabs = c("PC1","PC2","PC3","PC4")
par(mfrow=c(1,K))
for(i in 1:K){ #pc1*pc2 indicates melanoma is different from others, pc2*pc3 differentiate melanoma and leukemia, and pc3*pc4 for breast cancer...
  j = i+1
  plot(U[,i],U[,j],type="n",xlab=pclabs[i],ylab=pclabs[j])
  text(U[,i],U[,j],colnames(X),col=cols)
}

pc_scores <- data.frame(pc_new$x, check.rows = F) # check.rows = T gives .1 to duplicated row names
combinations <- combn(1:4, 2) # 4 is the number of pcs
p_list <- lapply(1:ncol(combinations), function(i) {
  combination <- combinations[,i,drop = T]
  
  p <- ggplot(pc_scores, aes_string(x=paste0("PC", i), y=paste0("PC", j))) +
    geom_point() + 
    geom_text(label=rownames(pc_scores)) +
    theme_minimal()
  return(p)
})

# PC loadings - visualize data by limiting to top genes in magnitude in the PC loadings (melanoma)
# For bi-clustering
# aa = grep("grey",colors()) #gene been turned way off
# bb = grep("blue",colors()) 
# cc = grep("orange",colors()) #gene been turned way on # 15
# gcol2 = colors()[c(aa[1:30],bb[1:20],rep(cc,2))]  # 80 colors, 

gcol2 <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(
  min(length(unique(as.vector(X))), 256)) # if only pos, "Oranges". If only neg, "Blues"

j = 2 # feature is the col
# ord = order(abs(V[,j]),decreasing=TRUE) # feature ordering
ord <- order(abs(pc_new$rotation[, j]), decreasing = T) # feature ordering
x = as.matrix(X[ord[1:250],]) #take 250 largest PCs, look at the melanoma pattern
h <- heatmap(x, scale = "none", col=gcol2)

### example begin ### 
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
distCor <- function(x) as.dist(1-cor(x)) # col correlation
zClust <- function(x, scale="row", zlim=c(-3,3), method="average") {
  if (scale=="row") z <- t(scale(t(x)))
  if (scale=="col") z <- scale(x)
  if(!is.null(z)) {
    z <- pmin(pmax(z, zlim[1]), zlim[2])
  }
  hcl_row <- hclust(distCor(t(z)), method=method)
  hcl_col <- hclust(distCor(z), method=method)
  return(list(data=z, Rowv=as.dendrogram(hcl_row), Colv=as.dendrogram(hcl_col)))
}

z <- zClust(data)

# require(RColorBrewer)
cols <- colorRampPalette(brewer.pal(10, "RdBu"))(256)

heatmap.2(z$data, trace='none', col=rev(cols), Rowv=z$Rowv, Colv=z$Colv)
### end example ###

# retrive dendrogram info 2March2021
h2 <- heatmap.2(x, scale = "none", col = gcol2, trace = "none")
col.clusters2 <- as.hclust(h2$colDendrogram ) # or col.clusters2 <- hclust(dist(t(x)))
cutree(col.clusters2, k = 3) # break into k=3 clusters
# to choose the best cutoff: https://uc-r.github.io/hc_clustering

# # retrive dendrogram info 23Feb2021
# h <- heatmap(x, scale = "none", col=gcol2, keep.dendro = T)
# h
# legend(x="right", legend=c("min", "ave", "max"), fill=gcol2)
# col.clusters = as.hclust( h$Colv )
# cutree(col.clusters,k=3) # break into k=3 clusters
# #or
# col.clusters = hclust(dist(t(x)))

# Variance Explained
varex = 0
cumvar = 0
denom = sum(D^2)
for(i in 1:64){
  varex[i] = D[i]^2/denom
  cumvar[i] = sum(D[1:i]^2)/denom
}

# screeplot
par(mfrow=c(1,2))
plot(1:64,varex,type="l",lwd=2,xlab="PC",ylab="% Variance Explained")
plot(1:64,cumvar,type="l",lwd=2,xlab="PC",ylab="Cummulative Variance Explained")

#######
