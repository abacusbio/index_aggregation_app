# Simulate EBV EV and index weights for Bayer corn breeding project
# Luna Zhang 2021

library(dplyr)
library(readxl)
library(corpcor)

# read inputs
inputDir <- "simulation/input/"
# sheet names: "EVs" "EV quantile" "Performance" "Pathology", "BLUP cor" "name match"

ebv <- lapply(c("Performance", "Pathology"), function(x) {
  perf <- readxl::read_xlsx(paste0(inputDir, "trait stats.xlsx"), sheet = x)
  r <- perf[,1,drop = T]
  perf <- data.frame(t(perf[,-1]))
  names(perf) <- r
  perf <- select(perf, mean, median, std.dev, min, max)
  return(perf)
})
ebv <- do.call(rbind, ebv)
ebv <- mutate(ebv, trait = rownames(ebv)) %>% 
  filter(trait!="ERSR_DIPDMA_BLUP")


corr <- readxl::read_xlsx(paste0(inputDir, "trait stats.xlsx"), sheet = "BLUP cor")
traitNames <- corr[,1,drop = T]
match(traitNames, ebv$trait) # sanity check
corr <- filter(corr, `...1`!="ERSR_DIPDMA_BLUP") %>% 
  select(-matches("ERSR_DIPDMA_BLUP|1$"))
traitNames <- traitNames[traitNames!="ERSR_DIPDMA_BLUP"]

## make ebv order the same as correlation order
ebv <- ebv[match(traitNames, ebv$trait),]

# simulate
## define paramters
cortolerance <- 1e-5
sireNumber <- 1000

rho <- corpcor::make.positive.definite(corr)
round(rho-corr, 2) # sanity check

## Covariance matrix of correlated traits.
genotypicVariance <- ebv$std.dev^2
sigma <- sqrt(genotypicVariance %*% t(genotypicVariance)) * rho
sigma <- make.positive.definite(sigma)

set.seed(NULL)
## Specified multivariate normal distribution using Sigma, specifying mean is zero
SireBVs <- MASS::mvrnorm(sireNumber, ebv$mean, # rep(0, length(traitNames)),
                   sigma, tol=cortolerance, empirical = TRUE) # empirical: estimated mean and sd
dimnames(SireBVs) <- list(NULL, trait = traitNames)

## sanity check
hist(round(cor(SireBVs)-rho, digits = 3), breaks = 100)
View(round(cor(SireBVs)-rho, digits = 3))
cor(SireBVs)[2,2];rho[2,2]

round(apply(SireBVs, 2, mean) - ebv$median, 2)
# ebv$std.dev
round((apply(SireBVs, 2, mean) - ebv$median)/ebv$std.dev, 2)
round(apply(SireBVs, 2, sd) - ebv$std.dev, 2)
round(apply(SireBVs, 2, min) - ebv$min, 2)
round(apply(SireBVs, 2, max) - ebv$max, 2)

## create ID
id <- 1:sireNumber
sex <- sample(c("M", "F"), sireNumber, replace = T)
dat <- data.frame(ID = id, sex = sex, SireBVs)

write.csv(dat, "c:/Users/lzhang/OneDrive - AbacusBio Ltd/GitHub/test_pca/simulation/sire_bv.csv",
           row.names = F)
