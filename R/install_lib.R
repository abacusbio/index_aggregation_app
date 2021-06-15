
# # get the list of installed packages and save it in data
# test <- sessionInfo()
# 
# m <- cbind(unlist(lapply(test$otherPkgs, `[[`, "Package")), 
#            unlist(lapply(test$otherPkgs, `[[`, "Version")))
# 
# m1 <- cbind(unlist(lapply(test$loadedOnly, `[[`, "Package")), 
#            unlist(lapply(test$loadedOnly, `[[`, "Version")))
# 
# write.table(rbind(m, m1), "R/data/lib_list.txt", row.names = F)

# read the lib list and install it in Domino
if(!"devtools" %in% installed.packages()[,"Package"]) {
  install.packages("devtools")
}

setwd("/repos/index_aggregation_app/")
test <- read.table("R/data/lib_list.txt", header = T)

library(dplyr)

test$lib <- paste(test$V1, test$V2, sep = "_") # libs needed
old_lib <- data.frame(V1 = installed.packages()[,"Package"],
                      V2 = installed.packages()[,"Version"],
                      lib = paste(installed.packages()[,"Package"], installed.packages()[, "Version"], sep = "_"),
                      LibPath = installed.packages()[,"LibPath"]
) %>% arrange(V1, V2) # sort by version old to new
idx <- which(duplicated(old_lib$V1, fromLast = T)) # remove older version duplicates
if(length(idx) > 0) old_lib <- old_lib[-idx,]

idx <- which(is.na(match(test$lib, old_lib$lib))) # find missing libs to install
idx_v_ck <- which(!is.na(match(test$V1, old_lib$V1))) # same lib can be different version

if(length(idx) > 0 && length(idx_v_ck) > 0) {
  
}

if(length(idx) > 0) {
  lapply(idx, function( i ) {
    devtools::install_version(package = test$V1[i], version = test$V2[i], 
                              repos = "http://cran.us.r-project.org")
  })
}

