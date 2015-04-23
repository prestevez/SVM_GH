## Using lapply to embedd the time series to many links

m <- 1
interval <- NULL
col <- c(1:2)
W <- W1
data <- flowdata

data_l <- lapply(1:length(col), function(i, m, interval, W, data, col){
          st_embed(data=data, m=m, col=col[i], W=W, ii=TRUE, interval=interval)},
          m=1, interval=interval, col=col, W=W, data=data)





# Separate the embedded time series in training and validation data


day_len <- 181
train_d <- 12


trainp <- length(data[,1]) - interval[h]*m[a] - traind*181


Xtr <- st_data$X[1:trainp,]
ytr <- st_data$y[1:trainp]
Xts <- st_data$X[(trainp+1):nrow(st_data$X),]
yts <- st_data$y[(trainp+1):nrow(st_data$y)]
