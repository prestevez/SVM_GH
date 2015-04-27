## Search script

load(".RData")

library(kernlab)
library(parallel)

# Load functions
source("kernel_methods_lib.R")
source("rmse_funs.R")
source("no_loop_funs.R")

# static params
W <- W1
day_len <- 181
train_d <- 12

# best paramaters
sigma <- 0.05
C <- 50
ep <- 0.5
col <- 1
# m <-
interval <- c(1, 181, 181*7)

# Fourth search
data3 <- m_embed(data=flowdata, m=m, col=col, interval=interval, W=W)

period3 <- trainp(x=day_len, y=train_d, data=data3, interval=interval, m=m)

spdata3 <- splitdata(x=day_len, y=train_d, data=data3, interval=interval, m=m)

save.image()

models4 <- svm_search_mint(data=spdata3, period=period3, sigma=sigma, C=C, epsilon=ep)

save.image()

errorlist4 <- modelerrors(models=models4, data=spdata3)

save.image()

results4 <- tabresults(errorlist4, period3)

save.image()
