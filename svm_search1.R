## First SVM Search: Best Parameters

# sigma Auto
m <- c(1)
interval <- c(1) # can embedd more than one interval at a time
col <- c(1)
W <- W1
day_len <- 181
train_d <- 12
C <- c(1, 10, 50, 100)
ep <- c(0.01, 0.1, 0.5)


data1 <- m_embed(data=flowdata, m=m, col=col, interval=interval, W=W)

period1 <- trainp(x=day_len, y=train_d, data=data1, interval=interval, m=m)

spdata1 <- splitdata(x=day_len, y=train_d, data=data1, interval=interval, m=m)

# modifed svm_search to use mclapply in C
svm_search_mcoreC <- function(data, period, sigma=NULL, C=1, epsilon=0.1)
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
  list <- lapply(1:length(data), function(dd, data, period, sigma, C, epsilon)
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
                                        list4 <- mclapply(1:length(C), function(c, Xtr, ytr, kp, C, epsilon)
                                        {
                                          enames <- paste("epsilon: ", as.character(epsilon), sep="")
                                          list5 <- lapply(1:length(epsilon),
                                                          function(e, Xtr, ytr, kp, C, epsilon)
                                                          {
                                                            models <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                                                                           kpar=kp, C=C, epsilon=epsilon[e], cross=5)
                                                          },Xtr=Xtr, ytr=ytr, C=C[c], epsilon=epsilon, kp=kp)
                                          setNames(list5, enames)
                                        }, Xtr=Xtr, ytr=ytr, kp=kp, C=C, epsilon=epsilon,
                                        mc.cores=detectCores())
                                        setNames(list4, cnames)
                                      }, Xtr=Xtr, ytr=ytr, sigma=sigma, C=C, epsilon=epsilon, kp=kp)
                      setNames(list3, snames)
                    }, data=data[[dd]], period=period, sigma=sigma, C=C, epsilon=epsilon,
                    odd=odd, even=even)
    setNames(list2, pnames)
  }, data=data, period=period, sigma=sigma, C=C, epsilon=epsilon)
  setNames(list, names)
}

# Run first search

system.time(
  models1 <- svm_search_mcoreC(data=spdata1, period=period1, sigma=NULL, C=C, epsilon=ep)
)

# errors from first search

system.time(
  errorlist1 <- modelerrors(models=models1, data=spdata1)
)

## Search using different values for sigma

sigma=c(0.05, 0.5, 1, 1.5, 3)
C=c(1, 10, 50)

# modifed svm_search to use mclapply in sigma
svm_search_mcoreSigma <- function(data, period, sigma=NULL, C=1, epsilon=0.1)
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
  list <- lapply(1:length(data), function(dd, data, period, sigma, C, epsilon)
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
                      list3 <- mclapply(1:length(sigma),
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
                                      }, Xtr=Xtr, ytr=ytr, sigma=sigma, C=C, epsilon=epsilon, kp=kp,
                                      mc.cores=detectCores())
                      setNames(list3, snames)
                    }, data=data[[dd]], period=period, sigma=sigma, C=C, epsilon=epsilon,
                    odd=odd, even=even)
    setNames(list2, pnames)
  }, data=data, period=period, sigma=sigma, C=C, epsilon=epsilon)
  setNames(list, names)
}

# Run second search

system.time(
  models2 <- svm_search_mcoreSigma(data=spdata1, period=period1, sigma=sigma, C=C, epsilon=ep)
)

# errors from second search

system.time(
  errorlist2 <- modelerrors(models=models2, data=spdata1)
)
