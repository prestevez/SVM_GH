## Begin Functions.R

# RMSE function

rmse<-function(obs, pred, na.rm=TRUE)
{sqrt(mean((obs-pred)^2, na.rm=na.rm))}

# End of RMSE function

# Begin NRMSE function

NRMSE <- function(res=NULL, obs, pred, sd=TRUE, na.rm=F)
{
  #Inputs:
  #res = vector of residuals
  #obs = vector of observed values
  #pred = vector of predicted values
  #sd = should standard deviation be used for normalisation? If FALSE, min max is used
  #na.rm = how should missing values (NA) be treated.

  if(is.null(res))
  {
    res <- obs-pred
  }
  if(sd==T)
  {
    NRMSE <- (sqrt(mean((res)^2, na.rm=na.rm)))/(sd(obs))
  }
  else
  {
    NRMSE <- (sqrt(mean((res)^2, na.rm=na.rm)))/(max(obs, na.rm=na.rm)-min(obs, na.rm=na.rm))
  }
  return(NRMSE=NRMSE)
}

# End of NRMSE function

# Kernel methods practical library

embed <- function(data, m, s=FALSE, avg=FALSE, int=FALSE, std=FALSE, interval=NULL)
{

  # A function to embed time series data

  # Inputs:
  # data: The data to be embedded
  # m: The embedding dimension
  # s: The length of the seasonal component (optional)
  # avg: Is an average to be added to the vectors? (optional)
  # int: Is an intercept term required? (optional)
  # std: Should the data be standardized? (options: "minmax")
  # interval: The time interval (spacing) of the series

  data <- as.matrix(data)
  if (!is.null(interval))
  {
    start <- 1
    end <- nrow(data)-(interval*m)
    ts <- matrix(0, end, m+1)

    for(i in 1:(m+1))
    {
      ts[,i] <- data[start:end,]
      start <- start+interval
      end <- end+interval
    }
  }
  else
  {
    interval <- 1
    ts <- matrix(0, nrow(data)-m, m+1)
    for (i in 1:(m+1))
    {
      ts[,i] <- as.matrix(data[i:(nrow(data)-(m-(i-1))),])
    }
  }
  y <- as.matrix(ts[,ncol(ts)])
  X <- as.matrix(ts[,1:(ncol(ts)-1)])

  if (s!=FALSE)
  {
    if(avg==TRUE)
    {
      avgProf <- matrix(average_profiles(as.matrix(data), s), nrow(data),)
      avgProf <- avgProf[-c(1:m*interval),]
      X <- cbind(X, avgProf)
    }
    else
    {
      X <- cbind(X, 1:s)
    }
  }
  if (std==TRUE)
  {
    X <- minmax(X)$mmmat
  }
  if (int==TRUE)
  {
    X <- cbind(1, X)
  }

  return(list(X=X, y=y))
}

# 1.8 st_embed

st_embed <- function(data, m, col, W, int=F, ii=T)
{

  y <- embed(as.matrix(data[,col]), m=m, int=FALSE)$y

  if(ii==TRUE)
  {
    W <- W+diag(1, nrow(W), ncol(W))
  }
  nbrs <- which(W[col,]>0)
  X <- NULL
  for (i in 1:length(nbrs))
  {
    if(is.null(X))
    {
      X <- embed(as.matrix(data[,nbrs[i]]), m=m, int=FALSE)$X
    }
    else
    {
      Xi <- embed(as.matrix(data[,nbrs[i]]), m=m, int=FALSE)$X
      X <- cbind(X, Xi)
    }
  }
  if (int==TRUE)
  {
    X <- cbind(1, X)
  }
  return(list(X=X, y=y))
}


# 1.9 minmax

minmax <- function(data, lb=0, ub=1, na.rm=TRUE)
{
  mmmat <- matrix(0, nrow(data), ncol(data))
  colmin <- apply(data, 2, min, na.rm=na.rm)
  colmax <- apply(data, 2, max, na.rm=na.rm)
  maxmin <- colmax-colmin
  bound <- (ub-lb)+lb
  for (i in 1:nrow(data))
  {
    mmmat[i,] <- ((data[i,]-colmin)/(maxmin))*bound
  }
  return(list(mmmat=mmmat, mins=colmin, maxs=colmax))
}

# 1.10 minmax_rev

minmax_rev <- function(data, lb=0, ub=1, mins, maxs)
{
  mmrmat <- data/((ub-lb)+lb)
  range <- maxs-mins
  for(i in 1:nrow(data))
  {
    mmrmat[i,] <- (mmrmat[i,]*range)+mins
  }
  return(mmrmat=mmrmat)
}

### End Kernel methods practical library

# Multiple plot function
#
# Taken from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## End of Multiple plot function

## svm_test function to iterate over several parameters

svm_test <- function(data=NULL, m=NULL, col=NULL, W=NULL, sig=NULL,
                     ii=TRUE, C=10, epsilon=0.1, cross=5,
                     trainp=NULL, outWD=NULL, mainWD=NULL,
                     nrmse=FALSE, graphs=FALSE, save=FALSE,
                     errplot=FALSE)
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
  # errplot: Logical, if TRUE graphs for error rates will be produced


  # Create empty data frame to store results

  svm_results <- data.frame("Model ID"=0, "Minutes"=0, "Link"=0, "Sigma"=0,
                            "C"=0, "Epsilon"=0, "Train Error"=0, "Residual Error"=0,
                            "Predicted Error"=0)

  colnames(svm_results)[7:9] <- c("Training Error" ,"Residual Error",
                                  "Predicted Error")

  # Create empty data frame to store best results

  best_results <- data.frame("Model ID"=0, "Minutes"=0, "Link"=0, "Sigma"=0,
                            "C"=0, "Epsilon"=0, "Train Error"=0, "Residual Error"=0,
                            "Predicted Error"=0)

  colnames(best_results)[7:9] <- c("Training Error" ,"Residual Error",
                                                            "Predicted Error")

  # Create mean_errors data frame
  mean_errors <- data.frame("Training Error"=0, "Residual Error"=0, "Predicted Error"=0)

  # For best_results table
  link.names <- colnames(data)


  # If NRMSE is TRUE

  if (nrmse==TRUE)
  {
    rmse <- NRMSE

    # Modify data frame to store results
    colnames(svm_results)[8:9] <- c("Standard Residual Error", "Standard Predicted Error")
    colnames(best_results)[8:9] <- c("Standard Residual Error", "Standard Predicted Error")
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
            tr_rmse <- rmse(pred=tr_pred, obs=ytr)
            ts_pred <- predict(model, Xts)
            ts_rmse <- rmse(obs=yts, pred=ts_pred)
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

            # Save objects
            if (save==TRUE)
            {
              assign(paste("pred.model", ind, sep=""), ts_pred)
              assign(paste("model", ind, sep=""), model)
            }

            # Graphs
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


            ind <- ind + 1
          }

          else
          {
            for (i in 1:length(sig))
            {
              kp <- list(sigma=sig[i])

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
              tr_rmse <- rmse(pred=tr_pred, obs=ytr)
              ts_pred <- predict(model, Xts)
              ts_rmse <- rmse(obs=yts, pred=ts_pred)
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

              # Save objects
              if (save==TRUE)
              {
                assign(paste("pred.model", ind, sep=""), ts_pred)
                assign(paste("model", ind, sep=""), model)
              }

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


              ind <- ind + 1
            }
          }
        }
      }
    }
  }

  # Select best results into a data frame
  for (g in 1:length(link.names))
  {
    best_results <- subset(svm_results, Link=link.names[g])
    best_results <- best_results[which(best_results[,7]==min(best_results[,7])),]
  }

#  # Create average error table
#  mean_errors <- best_results[1,7:9]
#  mean_errors <- mean_errors[-1,]

  if (nrmse==TRUE)
  {
    colnames(mean_errors)[2:3] <- c("Standard Residual Errors", "Standard Predicted Errors")
  }

  for (e in 1:3)
  {
    mean_errors[1,e] <- mean(best_results[,6+e])
  }

  rownames(mean_errors) <- "Average"

  # Plot mean errors of best models
  if (errplot==TRUE)
  {
    tr <- best_results[,7]
    err <-best_results[,8]
    pre <-best_results[,9]

    xres <- "Residual Errors"
    yres <- "Predicted Errors"
    rmlab <- "RMSE"

    if (nrmse==TRUE)
    {
      xres <- "Standard Residual Errors"
      yres <- "Standard Predicted Errors"
      rmlab <- "NRMSE"
    }

    ep1 <- qplot(err, pre, data=best_results, xlab=xres, ylab=yres) +
            ggtitle("SVR Models: Errors Summary")

    ep2 <- qplot(err, tr, data=best_results, xlab=xres, ylab="Training Errors",) +
            ggtitle("SVR Models: Errors Summary")


    nrmse.plot <- data.frame(best_results[,8])
    nrmse.plot[,2] <- xres

    nrmse.plot[nrow(nrmse.plot)+1:(nrow(nrmse.plot)+nrow(best_results)),1] <- best_results[,9]
    nrmse.plot[nrow(nrmse.plot)+1:(nrow(nrmse.plot)+nrow(best_results)),2] <- yres

    colnames(nrmse.plot) <- c("values", "type")

    ep3 <- qplot(type, values, data=nrmse.plot, geom="boxplot", ylab=rmlab, xlab="")+
            ggtitle("SVR Models: Errors Summary")


    terr.plot <- data.frame(best_results[,7])
    terr.plot[,2] <- "Training Errors"
    colnames(terr.plot) <- c("values", "type")

    ep4 <- qplot(type, values, data=terr.plot, geom="boxplot", ylab="Training Errors",
            xlab="") + ggtitle("SVR Models: Errors Summary")

    errplotfile <- paste("errorplots", length(col), "_links", ".pdf", sep="")

    setwd(outWD)
    pdf(errplotfile, width=11.7, height=8.3)
    multiplot(ep1, ep3, ep2, ep4, cols=2)
    dev.off()
    setwd(mainWD)
  }

  # Print results at prompt
  return(list(svm_results=svm_results, best_results=best_results,
              mean_errors=mean_errors))

}

## End of svm_test function
