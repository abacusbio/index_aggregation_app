
# get the list of installed packages and save it in data
test <- sessionInfo()

m <- cbind(unlist(lapply(test$otherPkgs, `[[`, "Package")), 
           unlist(lapply(test$otherPkgs, `[[`, "Version")))

m1 <- cbind(unlist(lapply(test$loadedOnly, `[[`, "Package")), 
           unlist(lapply(test$loadedOnly, `[[`, "Version")))

write.table(rbind(m, m1), "R/data/lib_list.txt", row.names = F)

# read the lib list and install it in Domino
if(!"devtools" %in% installed.packages()[,"Package"]) {
  install.packages("devtools")
}

setwd("/repo/index_aggregation_app/")
test <- read.table("R/data/lib_list.txt", header = T)

test$lib <- paste(test$V1, test$V2, sep = "_") # libs needed
old_lib <- paste(installed.packages()[,"Package"], installed.packages()[, "Version"], sep = "_")

idx <- which(is.na(match(test$lib, old_lib))) # find missing libs to install

if(length(idx) > 0) {
  lapply(idx, function( i ) {
    devtools::install_version(package = test$V1[i], version = test$V2[i], 
                              repos = "http://cran.us.r-project.org")
  })
}

