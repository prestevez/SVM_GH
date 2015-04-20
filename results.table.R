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


false.data <- c("X1384", "X2007", "X2140", "X2301")

results.summary.real <- subset(results.summary, !(Link %in% false.data))

results.summary.real
length(results.summary.real[,1])

model.errors <- results.summary.real[1,5:7]
model.errors <- model.errors[-1,]

for (i in 1:3)
{
  model.errors[1,i] <- mean(results.summary.real[,4+i])
}

rownames(model.errors) <- "Average"
model.errors





kable(results.summary, row.names=FALSE, digits=(4), format="markdown")
