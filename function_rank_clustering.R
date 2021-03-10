# how to render progress to shiny ui https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui
# rankcluster: https://cran.r-project.org/web/packages/Rankcluster/vignettes/Rankcluster.pdf

library(Rankcluster) # This algorithm results heavly depend on parameters

# sim input data
test <- matrix(rnorm(20, 0, 1), 100, 20) # a index by animal standardised matrix
test_rank <- t(apply(apply(test, 1, order, decreasing = T), 2, order)) # rank of each row, higher
# value is higher rank/smaller rank value

data("words")
test_rank <- words$data # 5 ranks, 98 people

start <- Sys.time()
re <- Rankcluster::rankclust(test_rank, m = ncol(test_rank), # size of the ranking
                             K = 1:ncol(test_rank), # test the best K using smallest BIC criterion
                             criterion = "bic", 
                             Qsem = 600, # total n iteration
                             Bsem = 100, # burn-in period
                             Ql = 500, # n interations to estimate log likelihood
                             Bl = 50, # burn-in period to estimate log likelihood
                             maxTry = 20, # max restarts if not converged
                             run = 3, #10, # n runs for each K
                             detail = T # verbose
)
print(Sys.time()-start) 
# 6   min! 10 runs for 5 ks (50 in total)
# 2.3 min 3 runs for 5 ks

k <- which.min(sapply(re@results, function(x) x@bic)) # only works when there's no NULL
# 567.0192 541.9801 541.9895 (530.7184)          when Bse=1, Bl = 50, run = 1
# 556.4146 535.0246 (525.4282) 526.4224          when Bse=100, Bl=50, run=1
# 558.3359 531.0145 (524.7702) 530.9254 532.7750 when Bse=100, Bl=50, run=3
# 556.4661 534.4233 (526.3253) 534.2438 535.4443 when Qsem = 600, Bse=100, Bl=50, run=3 (partition may be different)

# use Bsem = 1 to check burn-in 
# re@results[[k]]@distanceZ # length = Qsem - Bsem
test <- re@results[[k]]@distanceZ
plot(1:length(test), test)

# 1st 10% mean vs. last 10% mean
t.test(test[1:round(1000/10, 0)], test[(1000-round(1000/10, 0)):1000], 
       alternative = "less")$p.value < 0.05
# if TRUE,  then burn-in period too low

re@results[[k]]@distanceProp # length = Qsem - Bsem
test <- do.call(rbind, re@results[[k]]@distanceProp)
plot(1:nrow(test), test[,1])
points(1:nrow(test), test[,2], col = "red")
points(1:nrow(test), test[,3], col = "green")

re@results[[k]]@distancePi # length = Qsem - Bsem
test <- do.call(cbind, re@results[[k]]@distancePi)
plot(1:ncol(test), test[1,])
points(1:ncol(test), test[2,], col = "red")
points(1:ncol(test), test[3,], col = "green")


re@results[[k]]@partialRankScore # length = Qsem - Bsem
