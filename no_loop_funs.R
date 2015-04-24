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
  lapply(1:length(period), function(dd, x, data)
    {
      data[[dd]][1:x[dd],]
    }, data=data, x=period)
}

tr_sets <- splitdata(x=day_len, y=train_d, data=data_small, interval=interval, m=m)




tr_sets <- lapply(1:length(period), function(dd, data, x)
  {
    data[[dd]][1:x[dd],]
  }, data=data_small, x=period)

length(period)

Xtr <- data_small[[1:16]][1:period[1:16],]


Xtr <- st_data$X[1:trainp,]
ytr <- st_data$y[1:trainp]
Xts <- st_data$X[(trainp+1):nrow(st_data$X),]
yts <- st_data$y[(trainp+1):nrow(st_data$y)]

