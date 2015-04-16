## Begin Functions.R

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


