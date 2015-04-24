## Using lapply to embedd the time series to many links, add m options, add interval options.

m <- c(1:2)
interval <- c(1:2)
col <- c(1:2)
W <- W1
data <- flowdata
day_len <- 181
train_d <- 12

# Multiple spatio_temporal embedding function, mclapply version, about twice as fast.
m_embed <- function(m, interval, col, W, data)
{
  mclapply(1:length(col), function(z, m, interval, W, data, col)
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
}

## better embedding function
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
  mclapply(1:length(m), function(mm, x, y, data, m, interval)
  {
    lapply(1:length(data), function(dd, x, y, data, m, interval)
    {
      nrow(data[[dd]]) - interval*m - x*y
    },
    x=x, y=y, data=data, interval=interval, m=m[mm])
  }, 
  x=x, y=y, data=data, interval=interval, m=m, mc.cores=detectCores())
}


# work in progress, single core
trainp <- function(x, y, data, m, interval)
{
  lapply(1:length(m), function(mm, x, y, data, m, interval)
    {
      lapply(1:length(interval), function(int, x, y, data, m, interval)
        {
          lapply(1:length(data), function(dd, x, y, data, m, interval)
            {
              nrow(data[[dd]]) - interval*m - x*y
            }, 
            x=x, y=y, data=data, interval=interval[int], m=m)
        },
        x=x, y=y, data=data, interval=interval, m=m[mm])
    }, 
    x=x, y=y, data=data, interval=interval, m=m)
}


period <- trainp(x=day_len, y=train_d, data=data_small, interval=interval, m=m)

# Splits up into training and testing sets
# working fun, mcore
splitdata <- function(x, y, data, m, interval)
{
  # Create training period
  period <- trainp(x=x, y=y, data=data, m=m, interval=interval)
  period <- unlist(period)
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

names <- sapply(1:length(even), function(dd, even, data_small) colnames(data_small[[even[dd]]]), 
       data_small=data_small, even=even)

data_small <- unlist(data_small, recursive = F)

names <-colnames(data)
