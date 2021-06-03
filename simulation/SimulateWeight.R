library(dplyr)
library(readxl)
library(corpcor)

# read inputs
ev <- read.csv("R/data/sire_ev.csv", header = T, check.names = F)
head(ev)

n_index <- length(unique(ev$Index))
group <- unique(ev$group)
w <- data.frame(group = group, weight = rnorm(length(group), 50, 20))
idx <- which(w$weight < 0)
w$weight[idx] <- -w$weight[idx]

w <- select(ev, Index, group) %>% left_join(w) %>% select(-group)
head(w)

write.csv(w, "R/data/index_weight.csv", quote = F, row.names = F)
