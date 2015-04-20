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






# function pool



# ## svm_test function to iterate over several parameters
# 
svm_mods <- function(data=NULL, m=NULL, col=NULL, W=NULL, sig=NULL,
                     ii=TRUE, C=10, epsilon=0.1, cross=5,
                     trainp=NULL, outWD=NULL, mainWD=NULL, 
                     nrmse=FALSE, graphs=FALSE)
{
  # Inputs:
  # data: Matrix for the SVM
  # m: Number of past observation to embed
  # col: columns in the data
  # W: Weight matrix
  # ii: leave at TRUE
  # C: Penalisation constant
  # epsilon: width of epsilon tube
  # cross: number of folds for k-fold cross validation
  # trainp: Training period
  # outWD: name of Output Working directory
  # mainWD: name of main WD
  # nrmse: Logical, if TRUE RMSE is calculated using NRMSE (standardised)
  # graphs: Logical, if TRUE graphs will be produced
  
  # Initialise variables for best error and best model
  best_model <- NULL
  best_error <- 1000000
  best_time <- NULL
  
  # Create empty data frame to store results
  
  svm_results <- data.frame("Model ID"=0, "Minutes"=0, "Link"=0, "Sigma"=0,
                            "C"=0, "Epsilon"=0, "Train Error"=0, "Residual Error"=0,
                            "Predicted Error"=0)
  
  colnames(svm_results)[7:9] <- c("Training Error" ,"Residual Error",
                                  "Predicted Error") 
  
  
  # If NRMSE is TRUE
  
  if (nrmse==TRUE)
  {
    rmse <- nrmse
    
    # Modify data frame to store results
    colnames(svm_results)[8:9] <- c("Standard Residual Error", "Standard Predicted Error") 
  }
  else
  {
    
  }
  
  ind <- 1
  # Cycle through parameters
  for (a in 1:length(m))
  {
    for (b in 1:length(col))
    {
      for (j in 1:length(C))
      {
        for (k in 1:length(epsilon))
        {
          if (is.null(sig))
          {
            kp <- "automatic"
            
            # Embed the time series
            st_data <- st_embed(data=data, m=m[a], col=col[b], W=W, ii=ii)
            
            # Separate the embedded time series in training and validation data
            Xtr <- st_data$X[1:trainp]
            ytr <- st_data$y[1:trainp]
            Xts <- st_data$X[(trainp+1):nrow(st_data$X)]
            yts <- st_data$y[(trainp+1):nrow(st_data$y)]
            
            model <- ksvm(x=Xtr, y=ytr, type="eps-svr", kernel="rbfdot",
                          kpar=kp, C=C[j], epsilon=epsilon[k],
                          cross=cross)
            
            # Create objects to include in results Table
            tr_error <- error(model)
            tr_pred <- predict(model, Xtr)
            tr_rmse <- rmse(tr_pred, ytr)
            ts_pred <- predict(model, Xts)
            ts_rmse <- rmse(yts, ts_pred)
            sig_val <- kpar(kernelf(model))$sigma
            link_name <- colnames(data)[col[b]]
            minutes <- (m[a])*5
            
            # Add data to results table
            svm_results[ind,1] <- ind
            svm_results[ind,2] <- minutes
            svm_results[ind,3] <- link_name
            svm_results[ind,4] <- sig_val
            svm_results[ind,5] <- C[j]
            svm_results[ind,6] <- epsilon[k]
            svm_results[ind,7] <- tr_error
            svm_results[ind,8] <- tr_rmse
            svm_results[ind,9] <- ts_rmse
            
            
            if (graphs==TRUE)
            {
              # Create and save graphs
              obs_values_tr <- data.frame(ytr, "Observed", 1:length(ytr))
              colnames(obs_values_tr) <- c("value", "class", "time")
              pred_values_tr <- data.frame(tr_pred, "Predicted", 1:length(tr_pred))
              colnames(pred_values_tr) <- c("value", "class", "time")
              
              trainval_df <- rbind(obs_values_tr, pred_values_tr)
              trainval_df_one <- subset(trainval_df, time %in% c(1:181))
              
              obs_values_ts <- data.frame(yts, "Observed", 1:length(yts))
              colnames(obs_values_ts) <- c("value", "class", "time")
              pred_values_ts <- data.frame(ts_pred, "Predicted", 1:length(ts_pred))
              colnames(pred_values_ts) <- c("value", "class", "time")
              
              testval_df <- rbind(obs_values_ts, pred_values_ts)
              testval_df_one <- subset(testval_df, time %in% c(1:181))
              
              p1 <- ggplot(data=trainval_df, aes(x=time, y=value, colour=class, 
                                                 linetype=class))+
                geom_line(size=0.4) +
                scale_color_manual(values=c("Observed"="black", "Predicted"="red")) +
                ggtitle(paste(minutes, " mins. ", link_name,". " ,"Training set: Model ", 
                              ind, sep="")) +
                xlab("") + ylab("Seconds per metre") +
                theme(legend.title=element_blank())
              
              p2 <- ggplot(data=trainval_df_one,
                           aes(x=time, y=value, colour=class, linetype=class))+
                geom_line(size=0.4) +
                scale_color_manual(values=c("Observed"="black", "Predicted"="red")) +
                ggtitle(paste(minutes, " mins. ", link_name,". " 
                              ,"One Day Training set: Model ",ind, sep="")) +
                xlab("") + ylab("Seconds per metre") +
                theme(legend.title=element_blank())
              
              p3 <- ggplot(data=testval_df, aes(x=time, y=value, colour=class, 
                                                linetype=class))+
                geom_line(size=0.4) +
                scale_color_manual(values=c("Observed"="black", "Predicted"="red")) +
                ggtitle(paste(minutes, " mins. ", link_name,". " ,"Testing set: Model ",
                              ind, sep="")) +
                xlab("") + ylab("Seconds per metre") +
                theme(legend.title=element_blank())
              
              p4 <- ggplot(data=testval_df_one,
                           aes(x=time, y=value, colour=class, linetype=class))+
                geom_line(size=0.4) +
                scale_color_manual(values=c("Observed"="black", "Predicted"="red")) +
                ggtitle(paste(minutes, " mins. ", link_name,". " ,
                              "One Day Testing set: Model ", ind, sep="")) +
                xlab("") + ylab("Seconds per metre") +
                theme(legend.title=element_blank())
              
              file <- paste(minutes, "mins", "_", link_name, "_", "Model", 
                            ind, ".pdf", sep="")
              
              setwd(outWD)
              pdf(file, width=11.7, height=8.3)
              multiplot(p1, p2, p3, p4, cols=2)
              dev.off()
              setwd(mainWD)
            }
            
            else
            {
              
            }
            
            
            if(tr_error < best_error)
            {
              best_error <- tr_error
              best_model <- model
              best_time <- paste(minutes, "minutes", sep=" ")
            }
            ind <- ind + 1
          }
          
        }
      }
    } 
  }
  
  return(list(best_model=best_model, best_error=best_error, best_time=best_time,
              svm_results=svm_results[order(svm_results[,3],svm_results[,7]),]))
  
}