# Models and predictions

setwd("Rdata")
load("SVM_GH.RData")
setwd(mainWD)

# Divide models into 5 mins, 30 mins and 60 mins

# Add id col

id <- seq(1:22)
res <- results.summary
res[,8] <- id

min5 <- subset(res, Minutes == 5)
min30 <- subset(res, Minutes == 30)
min60 <- subset(res, Minutes == 60)


## Loops

links5 <- min5$V8
links30 <- min30$V8
links60 <- as.numeric(min60$V8)

m5 <- 1
m30 <- 6
m60 <- 12

trainp <- 181*154


ptm <- proc.time() # to measure processing time
## for 60 mins
# Cycle through parameters
for (b in 1:length(links60))
{
  # Embed the time series
  st_data <- st_embed(data=flowdata, m=m60, col=links60[b], W=W1, ii=TRUE)
  
  # Separate the embedded time series in training and validation data
  Xtr <- st_data$X[1:trainp]
  ytr <- st_data$y[1:trainp]
  Xts <- st_data$X[(trainp+1):nrow(st_data$X)]
  yts <- st_data$y[(trainp+1):nrow(st_data$y)]
  
  model <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                kpar="automatic", C=10, epsilon=0.1,
                cross=5)
  
  # objects that will be saved
  ts_pred <- predict(model, Xts)
  
  assign(paste("pred", links60[b], sep=""), ts_pred)
  assign(paste("model", links60[b], sep=""), model)
  assign(paste("test.nrmse", links60[b], sep=""), NRMSE(yts, ts_pred))
  
}

## for 30 mins
# Cycle through parameters
for (b in 1:length(links30))
{
  # Embed the time series
  st_data <- st_embed(data=flowdata, m=m30, col=links30[b], W=W1, ii=TRUE)
  
  # Separate the embedded time series in training and validation data
  Xtr <- st_data$X[1:trainp]
  ytr <- st_data$y[1:trainp]
  Xts <- st_data$X[(trainp+1):nrow(st_data$X)]
  yts <- st_data$y[(trainp+1):nrow(st_data$y)]
  
  model <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                kpar="automatic", C=10, epsilon=0.1,
                cross=5)
  
  # objects that will be saved
  ts_pred <- predict(model, Xts)
  
  assign(paste("pred", links30[b], sep=""), ts_pred)
  assign(paste("model", links30[b], sep=""), model)
  assign(paste("test.nrmse", links30[b], sep=""), NRMSE(yts, ts_pred))
  
}

## for 5 mins
# Cycle through parameters
for (b in 1:length(links5))
{
  # Embed the time series
  st_data <- st_embed(data=flowdata, m=m5, col=links5[b], W=W1, ii=TRUE)
  
  # Separate the embedded time series in training and validation data
  Xtr <- st_data$X[1:trainp]
  ytr <- st_data$y[1:trainp]
  Xts <- st_data$X[(trainp+1):nrow(st_data$X)]
  yts <- st_data$y[(trainp+1):nrow(st_data$y)]
  
  model <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                kpar="automatic", C=10, epsilon=0.1,
                cross=5)
  
  # objects that will be saved
  ts_pred <- predict(model, Xts)
  
  assign(paste("pred", links5[b], sep=""), ts_pred)
  assign(paste("model", links5[b], sep=""), model)
  assign(paste("test.nrmse", links5[b], sep=""), NRMSE(yts, ts_pred))
  
}

finaltime <- proc.time() - ptm # Processing time

setwd(dirRdata)
save.image(file="SVM_GH.RData", safe=TRUE)

save(model1,
     model2,
     model3,
     model4,
     model5,
     model6,
     model7,
     model8,
     model9,
     model10,
     model11,
     model12,
     model13,
     model14,
     model15,
     model16,
     model17,
     model18,
     model19,
     model20,
     model21,
     model22,
     pred1,
     pred2,
     pred3,
     pred4,
     pred5,
     pred6,
     pred7,
     pred8,
     pred9,
     pred10,
     pred11,
     pred12,
     pred13,
     pred14,
     pred15,
     pred16,
     pred17,
     pred18,
     pred19,
     pred20,
     pred21,
     pred22,
     test.nrmse1,
     test.nrmse2,
     test.nrmse3,
     test.nrmse4,
     test.nrmse5,
     test.nrmse6,
     test.nrmse7,
     test.nrmse8,
     test.nrmse9,
     test.nrmse10,
     test.nrmse11,
     test.nrmse12,
     test.nrmse13,
     test.nrmse14,
     test.nrmse15,
     test.nrmse16,
     test.nrmse17,
     test.nrmse18,
     test.nrmse19,
     test.nrmse20,
     test.nrmse21,
     test.nrmse22,
     file="mods_preds.R") 

setwd(mainWD)
