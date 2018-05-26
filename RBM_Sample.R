test <- rbind(
              matrix(rep(c(1,0,0,1),100),nrow=100,ncol=4,byrow=TRUE),
              matrix(rep(c(0,1,1,0),100),100,4,byrow=TRUE),
              c(1,0,1,0)
  
)

##
# The last row is an outlier. Let us use RBM to detect that
##
library(deepnet)
#
# Train the Model first

model <- rbm.train(test,2,numepochs = 2,batchsize = 2)

#
# Now Run the Model on data to generate the output and get the Input back
#

output <- round(rbm.down( model, rbm.up(model,test)),2)
#

final <- cbind(test,"|",output)
final
