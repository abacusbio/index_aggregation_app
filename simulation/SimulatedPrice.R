library(dplyr)
library(readxl)
library(corpcor)

# read inputs
inputDir <- "simulation/input/"
# sheet names: "EVs" "EV quantile" "Performance" "Pathology", "BLUP cor" "name match"

ev_summary <- readxl::read_xlsx(paste0(inputDir, "trait stats.xlsx"), sheet = "EVs")
ev_q <- readxl::read_xlsx(paste0(inputDir, "trait stats.xlsx"), sheet = "EV quantile")
name_match <- readxl::read_xlsx(paste0(inputDir, "trait stats.xlsx"), sheet = "name match", 
                                col_names = c("ev", "type", "trait"), skip = 1, trim_ws = T, na = " ")

# generate data
## ev
test <- filter(ev_summary, `...1` %in% c("min", "median", "max", "nbr.val",  "nbr.null", "nbr.na")) 
r <- test$...1 
r[r=="min"] <- "q0"; r[r=="max"] <- "q100"
test <- select(test, -matches("1$")) %>% t()
test <- data.frame(rownames(test), test)
names(test) <- c("trait", r)

ev_summary <- left_join(ev_q, test, by = "trait")

## name
name_match <- select(name_match, -matches("type")) %>% 
  filter(!ev == "-" & !is.na(trait) & trait!="ERSR_DIPDMA")
name_match$trait <- gsub("\\s+", "", name_match$trait)
name_match$trait <- paste0(name_match$trait, "_BLUP")

## merge
ev_summary <- left_join(name_match, ev_summary, by = c("ev"="trait"))

# simulate without correlation
## define paramters
# cortolerance <- 1e-5
# indexNumber <- 1000
# 
# n_sec <- rep(round(indexNumber/4), 3)
# n_sec[4] <- indexNumber - sum(n_sec)
# names(n_sec) <- paste0("q", as.character(seq(0, 75, 25)))

indexNumber <- select(ev_summary, matches("nbr")) %>% apply(1, sum) %>% max()

q_names <- paste0("q", as.character(seq(0, 100, 25)))

set.seed(NULL)
ev <- lapply(1:nrow(ev_summary), function(i) {
  ev_row <- ev_summary[i,,drop = F]
  
  # allocation n to each quantile
  n_index <- ev_row$nbr.val + ev_row$nbr.null
  n_sec <- rep(round(n_index/4), 3)
  n_sec[4] <- n_index - sum(n_sec)
  names(n_sec) <- paste0("q", as.character(seq(0, 75, 25)))
  
  ev_q <- lapply(1:4, function(j) {
    
    x <- seq(from = ev_row[[q_names[j]]], to = ev_row[[q_names[j+1]]], 
             by = (ev_row[[q_names[j+1]]] - ev_row[[q_names[j]]])/(n_sec[j] - 1)
    )
    
    if(length(x) == n_sec[j]){
      replace = F
    } else { # from and to are the same number e.g. very skewed
      replace = T
    }
    
    ev <- sample(x, size = n_sec[j], replace = replace)  
    return(ev)
  })
  ev_q <- unlist(ev_q)
  
  # NA values
  n_na <- indexNumber - n_index
  ev_q <- c(ev_q, rep(0, n_na))
  ev_q <- sample(ev_q, size = length(ev_q), replace = F)
  return(ev_q)
})
names(ev) <- ev_summary$trait
test <- data.frame(do.call(cbind, ev))
round(cor(test), 2)

# simulate with correlation and skewed normal distribution
# https://www.r-bloggers.com/2020/11/skewness-and-kurtosis-in-statistics/ get lambda (kurtosis)
# https://cran.r-project.org/web/packages/MomTrunc/MomTrunc.pdf

test <- data.frame(Index = paste0("index_", 1:nrow(test)), test)
test$group <- sample(LETTERS[1:10], nrow(test), replace = T)
# http://koeppen-geiger.vu-wien.ac.at/pics/KG_USA_5min.pdf http://koeppen-geiger.vu-wien.ac.at/usa.htm
test$RM <- sample(c("Equatorial rainforest", "Equatorial monsoon", "Equatorial savanna w", 
                    "mid-latitude desert", "Subtropical dessert", 
                    "Mid-latitude steppe","Subtropical steppe", "Mediterranean b",
                    "Humid subtropical", "Humid continental a", "Humid continental b", "Sub-arctic"), 
                  nrow(test), replace = T,
                  prob = c(0.006, 0.006, 0.008, 
                           0.09, 0.09, 0.13, 0.09, 0.11,
                           0.2, 0.08, 0.12, 0.05))

write.csv(test, "c:/Users/lzhang/OneDrive - AbacusBio Ltd/GitHub/test_pca/simulation/sire_ev.csv",
          row.names = F)
