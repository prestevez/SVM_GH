## results.table.R

setwd(dirRdata)
load("SVM_GH.RData")

# Return to mainWD
setwd(mainWD)

# Best results table

res.table <- svm_search_nrmse$svm_results

temp <- res.table[,c(1:4, 7:9)]
temp.m <- temp[1,]
temp.m <- temp.m[-1,]
link.names <- colnames(flowdata)

for (i in 1:length(link.names))
{
  temp.l <- subset(temp, Link==link.names[i])
  temp.m[i,] <- temp.l[which(temp.l[,5]==min(temp.l[,5])),]
  rm(temp.l)
}

results.summary <- temp.m
rm(temp.m)
rm(temp)
rm(link.names)
rm(i)

# Remove links with historical data
false.data <- c("X1384", "X2007", "X2140", "X2301")
results.summary.real <- subset(results.summary, !(Link %in% false.data))

rm(false.data)

# Create a table with the average errors from all links
model.errors <- results.summary[1,5:7]
model.errors <- model.errors[-1,]

for (i in 1:3)
{
  model.errors[1,i] <- mean(results.summary[,4+i])
}

rownames(model.errors) <- "Average"

# Create a table with the average errors from all links with real data
model.errors.real <- results.summary.real[1,5:7]
model.errors.real <- model.errors[-1,]

for (i in 1:3)
{
  model.errors.real[1,i] <- mean(results.summary.real[,4+i])
}

rownames(model.errors.real) <- "Average"


## Print Tables in markdown
### Best model for each link
kable(results.summary, row.names=FALSE, digits=(4), format="markdown")

### Best model for each link (exluding those with historical data)
kable(results.summary.real, row.names=FALSE, digits=(4), format="markdown")

### Mean errors across best models
kable(model.errors, row.names=FALSE, digits=(4), format="markdown")

### Mean errors across best models (exluding those with historical data)
kable(model.errors.real, row.names=FALSE, digits=(4), format="markdown")


## Plots of errors (only of links with true data)


tr <- results.summary.real[,5]
err <- results.summary.real[,6]
pre <- results.summary.real[,7]

plot.errvpred <- qplot(err, pre, data=results.summary.real, xlab="Standard Residual Errors",
                       ylab="Standard Predicted Errors") + ggtitle("SVR Models: Errors Summary")

plot.trverr <- qplot(err, tr, data=results.summary.real, xlab="Standard Residual Errors",
                      ylab="Training Errors") + ggtitle("SVR Models: Errors Summary")


res.plot[37:(36+18),1] <- data.frame(results.summary.real[,7])
res.plot[37:(36+18),2] <- "Standard Predicted Errors"

colnames(res.plot) <- c("values", "type")
  
plot.nrmse <- qplot(type, values, data=res.plot[19:54,], geom="boxplot", 
                  ylab="NRMSE", xlab="") + ggtitle("SVR Models: Errors Summary")

plot.terr <- qplot(type, values, data=res.plot[1:18,], geom="boxplot",
                   ylab="Training Errors", xlab="") + ggtitle("SVR Models: Errors Summary")

fileplot <- "errorplots.pdf"

setwd(dirRoutput)
pdf(fileplot, width=11.7, height=8.3)
multiplot(plot.errvpred, plot.nrmse, plot.trverr, plot.terr, cols=2)
dev.off()
setwd(mainWD)



# Save objects
setwd(dirRdata)

save.image(file="SVM_GH.RData", safe=TRUE)
save(res.table, results.summary, results.summary.real, model.errors, model.errors.real,
     file="resultstables.RData")

# Return to mainWD
setwd(mainWD)


