

tr_sets[[1]]

ul <- unlist(tr_sets, recursive=FALSE)

system.time(models <- lapply(seq(1,length(ul)/4, 1), function(dd, data)
{
  Xtr <- data[[dd]]
  ytr <- data[[dd+1]]
  ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                 kpar="automatic", epsilon=0.1, cross=5)
},
data=ul)
)
  
# mclapply option
require(parallel)

system.time(models <- mclapply(seq(1,length(ul)/4, 1), function(dd, data)
{
  Xtr <- data[[dd]]
  ytr <- data[[dd+1]]
  ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
       kpar="automatic", epsilon=0.1, cross=5)
},
data=ul, mc.cores=detectCores())
)

## test with
