## Using lapply to embedd the time series to many links, add m options, add interval options.

m <- c(1)
interval <- c(1) # can embedd more than one interval at a time
col <- c(1:5)
W <- W1
data <- flowdata
day_len <- 181
train_d <- 12
C <- c(1)
ep <- c(0.1)

## better embedding function, multicore with names
m_embed <- function(m, interval, col, W, data)
{
  names <- colnames(data)[1:length(col)]
  list <- mclapply(1:length(col), function(z, m, interval, W, data, col)
  {
    unlist(lapply(1:length(m), function(mm, m, interval, W, data, col)
    {
      unlist(lapply(1:length(interval), function(int, m, interval, W, data, col)
      {
        st_embed(data=data, m=m, col=col, W=W, ii=TRUE, interval=interval[int])
      },
      m=m[mm], interval=interval, col=col, W=W, data=data), recursive=FALSE)
    },
    m=m, interval=interval, col=col[z], W=W, data=data), recursive=FALSE)
  },
  m=m, interval=interval, col=col, W=W, data=data, mc.cores=detectCores())
  setNames(list, names)
}

system.time(data_small <- m_embed(data=flowdata, m=m, col=col, interval=interval, W=W))

# Separate the embedded time series in training and validation data

# Generates the length of the training periods
# used in splitdata, but need to generate it for the svm
trainp <- function(x, y, data, m, interval)
{
  testp <- x*y
  mat_m <- t(matrix(m))
  mat_int <- matrix(interval)
  mat <- mat_int %*% mat_m
  n <- nrow(data[[1]][[1]])+1
  nl <- n - mat
  trainp <- nl-mat-testp
  trainp <- c(trainp)
  trainp <- t(matrix(trainp))
  trainp <- rbind(trainp, trainp)
  trainp <- c(trainp)
  m2 <- t(matrix(paste(as.character(m*5), "mins", sep="")))
  m2 <- rbind(m2, m2)
  m2 <- c(m2)
  int2 <- t(matrix(paste(as.character(interval), "int", sep="")))
  int2 <- c(int2, int2)
  int2 <- c(int2)
  names_mat <- matrix(0, length(m), length(interval))
  names_mat <- sapply(1:(length(m)*length(interval)), function(ll, m2, int2, names_mat){
    names_mat[ll] <- paste(m2[ll], int2[ll], sep="_")},
    m2=m2, int2=int2, names_mat=names_mat)
  names_mat <- t(matrix(names_mat))
  names_mat <- rbind(paste("X", names_mat, sep="_"), paste("y", names_mat, sep="_"))
  names_mat <- c(names_mat)
  names(trainp) <- names_mat
  return(trainp)
}

period <- trainp(x=day_len, y=train_d, data=data_small, interval=interval, m=m)

# Splits up into training and testing sets
splitdata <- function(x, y, data, m, interval)
{
  # Create training period
  period <- trainp(x=x, y=y, data=data, m=m, interval=interval)
  names <- names(data)
  # Start mclapply by link
  list <- mclapply(1:length(data), function(dd, period, data)
  {
    pnames <- names(period)
    # Training sets
    Training <- mclapply(1:length(period), function(p, period , data)
      {
        Tr <- data[[p]][1:period[p],]
      }, data=data[[dd]], period=period, mc.cores=detectCores())
    # Testing sets
    Testing <- mclapply(1:length(period), function(p, period, data)
      {
        data[[p]][(period[p]+1):nrow(data[[p]]),]
      }, data=data[[dd]], period=period, mc.cores=detectCores())
    names(Training) <- pnames
    names(Testing) <- pnames
    list(Training=Training, Testing=Testing)
  }, period=period, data=data, mc.cores=detectCores())
  setNames(list, names)
}

system.time(tr_sets <- splitdata(x=day_len, y=train_d, data=data_small, interval=interval, m=m))

# svm_search function

svm_search <- function(data, period, sigma=NULL, C=1, epsilon=0.1)
{
  # Inputs:
  # data: Previously prepared data using m_embed and splitdata functions
  # period: Length of training periods created with trainp function
  # sigma: sigma value for svm, defaults to NULL for automatic estimation using sigest
  # C: Penalisation constant, defaults to 1
  # epsilon: width of epsilon tube, dedaults to 0.1

  if (is.null(sigma))
  {
    sigma <- 1
  }
  names <- names(data)
  # Start mclapply per link
  list <- mclapply(1:length(data), function(dd, data, period, sigma, C, epsilon)
  {
    odd <- seq(1, length(period), 2)
    even <- seq(2, length(period), 2)
    pnames <- sub("X_", "", names(period))[c(odd)]
    # lapply by m*int combinations (reflected in half "period")
    list2 <- lapply(1:(length(period)/2),
      function(p, data, period, odd, even, sigma, C, epsilon)
        {
          Xtr <- data[[1]][[odd[p]]]
          ytr <- data[[1]][[even[p]]]
          if (sigma==1)
          {
            snames <- "Sigma: Auto"
            kp <- "automatic"
          }
          else
          {
            snames <- paste("sigma: ", as.character(sigma), sep="")
          }
          # mclapply by sigma
          list3 <- lapply(1:length(sigma),
          function(s, sigma, Xtr, ytr, C, epsilon, kp)
          {
            if (sigma!=1)
            {
              kp <- list(sigma=sigma[s])
            }
            cnames <- paste("C: ", as.character(C), sep="")
            list4 <- lapply(1:length(C), function(c, Xtr, ytr, kp, C, epsilon)
            {
              enames <- paste("epsilon: ", as.character(epsilon), sep="")
              list5 <- lapply(1:length(epsilon),
              function(e, Xtr, ytr, kp, C, epsilon)
              {
                models <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                    kpar=kp, C=C, epsilon=epsilon[e], cross=5)
              },Xtr=Xtr, ytr=ytr, C=C[c], epsilon=epsilon, kp=kp)
              setNames(list5, enames)
            }, Xtr=Xtr, ytr=ytr, kp=kp, C=C, epsilon=epsilon)
            setNames(list4, cnames)
          }, Xtr=Xtr, ytr=ytr, sigma=sigma, C=C, epsilon=epsilon, kp=kp)
          setNames(list3, snames)
        }, data=data[[dd]], period=period, sigma=sigma, C=C, epsilon=epsilon,
            odd=odd, even=even)
     setNames(list2, pnames)
  }, data=data, period=period, sigma=sigma, C=C, epsilon=epsilon,
      mc.cores=detectCores())
  setNames(list, names)
}

system.time(
models_fun <- svm_search(data=tr_sets, period=period, sigma=NULL, C=C, epsilon=ep)
)


# Create predictions NRMSE and RMSE for the models

modelerrors <- function(models, data)
{
  # models: list of ksvm models generated with svm_search function
  # data: the same list of data used to generates the models object
  
  # unlist the models and the data for easier managemet
  models  <- lapply(1:length(data), function(d, models)
                    {unlist(models[[d]], recursive=TRUE)},
                    models=models)
  data <- unlist(data, recursive=F)
  trid <- seq(1, length(data), 2)
  tsid <- seq(2, length(data), 2)
  tr <- data[c(trid)]
  ts <- data[c(tsid)]
  trnames <- names(tr)
  tsnames <- names(ts)
  Training <- mclapply(1:length(tr), function(t, tr, models)
    {
      Xtr <- tr[[t]][[1]]
      ytr <- tr[[t]][[2]]
      models <- models[[t]]
      mnames <- names(models)
      list2 <- lapply(1:length(models), function(m, models, Xtr, ytr)
        {
          sigma_val <- kpar(kernelf(models[[m]]))$sigma
          C_val <- param(models[[m]])$C
          eps_val <- param(models[[m]])$epsilon
          SV <- nSV(models[[m]])
          traine <- error(models[[m]])
          pred <- predict(models[[m]], Xtr)
          residual <- ytr - pred
          rmse <- rmse(obs=ytr, pred=pred)
          nrmse <- NRMSE(obs=ytr, pred=pred)
          list(pred=pred, residual=residual, sigma_val=sigma_val, C_val=C_val, SV=SV,
               eps_val=eps_val, traine=traine, rmse=rmse, nrmse=nrmse)
        }, models=models, Xtr=Xtr, ytr=ytr)
      setNames(list2, mnames)
    }, tr=tr, models=models, mc.cores=detectCores())
  names(Training) <- trnames
  Testing <- mclapply(1:length(ts), function(t, ts, models)
    {
      Xts <- ts[[t]][[1]]
      yts <- ts[[t]][[2]]
      models <- models[[t]]
      mnames <- names(models)
      list3 <- lapply(1:length(models), function(m, models, Xts, yts)
        {
          sigma_val <- kpar(kernelf(models[[m]]))$sigma
          C_val <- param(models[[m]])$C
          eps_val <- param(models[[m]])$epsilon
          SV <- nSV(models[[m]])
          pred <- predict(models[[m]], Xts)
          residual <- yts - pred
          rmse <- rmse(obs=yts, pred=pred)
          nrmse <- NRMSE(obs=yts, pred=pred)
          list(pred=pred, residual=residual, sigma_val=sigma_val, C_val=C_val, SV=SV,
               eps_val=eps_val, rmse=rmse, nrmse=nrmse)
        }, models=models, Xts=Xts, yts=yts)
      setNames(list3, mnames)
    }, ts=ts, models=models, mc.cores=detectCores())
  names(Testing) <- tsnames
#   return(list(Training=Training, Testing=Testing))
  return(list(Training, Testing))
}

system.time(
  errorlist <- modelerrors(models_fun, tr_sets)
  )


# Integrate results in a table

trdf <- data.frame(matrix(0, length(errorlist1[[1]][[1]]), 11))
colnames(trdf) <- c("Link", "m*int", "Sigma", "C", "Epsilon", "SV", 
                              "Tr.Err", "Tr.RMSE", "Tr.NRMSE", "Ts.RMSE", "Ts.NRMSE")

ind <- 1
for (i in 1:length(errorlist1[[1]]))
{
  for (b in 1:length(errorlist1[[1]][[i]]))
  {
    trdf[ind, 1] <- sub(".Training", "", names(errorlist1[[1]]))
    trdf[ind, 2] <- sub("X_", "", names(period1[1]))
    trdf[ind, 3] <- errorlist1[[1]][[i]][[b]]$sigma_val
    trdf[ind, 4] <- errorlist1[[1]][[i]][[b]]$C_val
    trdf[ind, 5] <- errorlist1[[1]][[i]][[b]]$eps_val
    trdf[ind, 6] <- errorlist1[[1]][[i]][[b]]$SV
    trdf[ind, 7] <- errorlist1[[1]][[i]][[b]]$traine
    trdf[ind, 8] <- errorlist1[[1]][[i]][[b]]$rmse
    trdf[ind, 9] <- errorlist1[[1]][[i]][[b]]$nrmse
    
    ind <- ind + 1
  }
}
ind <- 1
for (i in 1:length(errorlist1[[2]]))
{
  for (b in 1:length(errorlist1[[2]][[i]]))
  {
    trdf[ind, 10] <- errorlist1[[2]][[i]][[b]]$rmse
    trdf[ind, 11] <- errorlist1[[2]][[i]][[b]]$nrmse
    
    ind <- ind + 1
  }
}
 

# make process into a fun
tabresults <- function(errorlist, period)
{
  df <- data.frame(matrix(0, length(errorlist[[1]][[1]]), 11))
  colnames(df) <- c("Link", "m*int", "Sigma", "C", "Epsilon", "SV", 
                      "Tr.Err", "Tr.RMSE", "Tr.NRMSE", "Ts.RMSE", "Ts.NRMSE")
  
  ind <- 1
  for (i in 1:length(errorlist[[1]]))
  {
    for (b in 1:length(errorlist[[1]][[i]]))
    {
      df[ind, 1] <- sub(".Training", "", names(errorlist[[i]]))
      df[ind, 2] <- sub("X_", "", names(period[i]))
      df[ind, 3] <- errorlist[[1]][[i]][[b]]$sigma_val
      df[ind, 4] <- errorlist[[1]][[i]][[b]]$C_val
      df[ind, 5] <- errorlist[[1]][[i]][[b]]$eps_val
      df[ind, 6] <- errorlist[[1]][[i]][[b]]$SV
      df[ind, 7] <- errorlist[[1]][[i]][[b]]$traine
      df[ind, 8] <- errorlist[[1]][[i]][[b]]$rmse
      df[ind, 9] <- errorlist[[1]][[i]][[b]]$nrmse
      
      ind <- ind + 1
    }
  }
  ind <- 1
  for (i in 1:length(errorlist[[2]]))
  {
    for (b in 1:length(errorlist[[2]][[i]]))
    {
      df[ind, 10] <- errorlist[[2]][[i]][[b]]$rmse
      df[ind, 11] <- errorlist[[2]][[i]][[b]]$nrmse
      
      ind <- ind + 1
    }
  }
  return(df)
}
  

  
  