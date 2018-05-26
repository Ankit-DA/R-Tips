# https://www.kaggle.com/dalpozz/creditcardfraud
#
library(deepnet)
#
# Read the data
#
indata<-read.csv(file="C:\\Users\\ankitagarwal5\\Downloads\\creditcardfraud\\creditcard.csv",stringsAsFactors = FALSE, header = TRUE)
# 
str(indata)
testdata <-indata[1:30]
#
# Standardize all values between 0 and 1
#
normalize <- function(x) {
  return (round((x - min(x)) / (max(x) - min(x)),3))
}
#
normdata <- as.data.frame(lapply(testdata, normalize))
#
# Convert to matrix
#
normdata<-as.matrix(normdata)
#
# Train the RBM Model first

model <- rbm.train(normdata,4,numepochs = 20,batchsize = 50000)

#
# Now Run the Model on data to generate the output and get the Input back
#

output <- round(rbm.down( model, rbm.up(model,normdata)),3)
#
diff <- abs(output-normdata)
#
# perchange <- round((diff/normdata) * 100,2) results in Inf if normdata = 0 for a row/column value
# so converting 0 to .001 
#
#normdata[normdata==0] <- .001 
#
#perchange <- round((diff/normdata) * 100,2)
testsum<-rowSums(diff[,2:29])
#
test1<-indata[order(testsum,decreasing = TRUE),31]
#
sum(test1[1:1000])
