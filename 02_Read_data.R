# 02_Read_data.R

setwd(dirRawData)

# Read the time series data
ujt.df <- read.csv("ujt.csv", na.strings=c("",".","NULL"))

flowdata <- as.matrix(ujt.df[,1:22])

## Read the spatial weight matrices
W.df <- read.csv("Weight_Matrices.csv", header=TRUE,stringsAsFactors=FALSE)

W1 <- matrix(rep(0,22*22), nrow=22)
rownames(W1) <- W.df[1:22,1]
colnames(W1) <- colnames(W.df)[2:23]
W1[1:22,1:22] <- data.matrix(W.df[1:22,2:23])

W2 <- matrix(rep(0,22*22), nrow=22)
rownames(W2) <- W.df[24:45,1]
colnames(W2) <- colnames(W.df)[2:23]
W2[1:22,1:22] <- data.matrix(W.df[24:45,2:23])

W3 <- matrix(rep(0,22*22), nrow=22)
rownames(W3) <- W.df[47:68,1]
colnames(W3) <- colnames(W.df)[2:23]
W3[1:22,1:22] <- data.matrix(W.df[47:68,2:23])

# Save data objects

setwd(dirRdata)
save(ujt.df, flowdata, W.df, W1, W2, W3,
     file="SVM_GH.Rdata")

