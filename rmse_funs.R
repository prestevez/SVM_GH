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