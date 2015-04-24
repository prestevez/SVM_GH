## Using lapply to embedd the time series to many links, add m options, add interval options.

m <- c(1:2)
interval <- c(1,181) # can embedd more than one interval at a time, can't split well yet, though
col <- c(1:2)
W <- W1
data <- flowdata
day_len <- 181
train_d <- 12

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
# working fun, mcore
trainp <- function(x, y, data, m, interval)
{
  testp <- x*y
  mat_m <- t(matrix(m))
  mat_int <- matrix(interval)
  mat <- mat_int %*% mat_m
  n <- nrow(data[[1]][[1]])+1
  nl <- n - mat
  trainp <- nl-mat-testp
  c(trainp)
}

period <- trainp(x=day_len, y=train_d, data=data_small, interval=interval, m=m)

# Splits up into training and testing sets
# working fun, mcore
splitdata <- function(x, y, data, m, interval)
{
  # Create training period
  period <- trainp(x=x, y=y, data=data, m=m, interval=interval)
  # Training sets
  Training <- mclapply(1:length(period), function(dd, x, data)
  {
    data[[dd]][1:x[dd],]
  }, data=data, x=period, mc.cores=detectCores())

  # Testing sets
  Testing <- mclapply(1:length(period), function(dd, x, data)
  {
    data[[dd]][(x[dd]+1):nrow(data[[dd]]),]
  }, data=data, x=period, mc.cores=detectCores())
  return(unlist(list(Training=Training, Testing=Testing)))
}


## work in progress
splitdata <- function(x, y, data, m, interval)
{
  # Create training period
  period <- trainp(x=x, y=y, data=data, m=m, interval=interval)

  # Training sets
  Training <- lapply(1:length(period), function(dd, x, data)
    {
      data[[dd]][1:x[dd],]
    }, data=data, x=period)

  # Testing sets
  Testing <- lapply(1:length(period), function(dd, x, data)
    {
      data[[dd]][(x[dd]+1):nrow(data[[dd]]),]
    }, data=data, x=period)
  return(list(Training=Training, Testing=Testing))
}

system.time(tr_sets <- splitdata(x=day_len, y=train_d, data=data_small, interval=interval, m=m))







even <- seq(2, length(data_small), 2)
