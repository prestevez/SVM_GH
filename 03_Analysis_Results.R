# 03_Analysis_Results.R

svm_search <- svm_test(data=flowdata, m=c(1:3), col=c(1:3), W=W1, ii=TRUE, sig=NULL,
                       C=10, epsilon=0.1, cross=5, trainp=(181*154), outWD=dirRoutput,
                       mainWD=mainWD)

# Basic svm_search. Parameters accept several values, except W.
# The sig defaults to NULL for automatic sigma estimation.

## Results

svm_search

# markdown table of results

kable(svm_search$svm_results, format="markdown")

# End of 03_Analysis_Results.R