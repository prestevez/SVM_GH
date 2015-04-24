## Using lapply to embedd the time series to many links, add m options, add interval options.

m <- c(1:2)
interval <- c(1,6)
col <- c(1:2)
W <- W1
data <- flowdata

# Multiple spatio_temporal embedding function
m_embed <- function(m, interval, col, W, data)
{
  unlist(lapply(1:length(col), function(z, m, interval, W, data, col)
  {
    unlist(lapply(1:length(interval), function(int, m, interval, W, data, col)
    {
      unlist(lapply(1:length(m), function(mm, m, interval, W, data, col)
      {
        st_embed(data=data, m=m[mm], col=col, W=W, ii=TRUE, interval=interval)
      },
      m=m, interval=interval[int], col=col, W=W, data=data), recursive=FALSE)
    },
    m=m, interval=interval, col=col[z], W=W, data=data), recursive=FALSE)
  },
  m=m, interval=interval, col=col, W=W, data=data), recursive=FALSE)
}


system.time(data_small <- m_embed(data=flowdata, m=m, col=col, interval=interval, W=W))


# Separate the embedded time series in training and validation data

day_len <- 181
train_d <- 12
m <- c(1)
interval <- c(1)

# Generates the length of the training periods
trainp <- function(x, y, data, m, interval)
{
  unlist(lapply(1:length(m), function(mm, x, y, data, m, interval)
    {
      lapply(1:length(data), function(dd, x, y, data, m, interval)
        {
          nrow(data[[dd]]) - interval*m - x*y
        },
        x=x, y=y, data=data, interval=interval, m=m[mm])
    }, x=x, y=y, data=data, interval=interval, m=m))
}


period <- trainp(x=day_len, y=train_d, data=data_small, interval=interval, m=m)

# Splits up into training and testing sets
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

tr_sets <- splitdata(x=day_len, y=train_d, data=data_small, interval=interval, m=m)




