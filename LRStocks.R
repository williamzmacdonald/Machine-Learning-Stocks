rm(list=ls())
LR1D <- function(tx,ty) { #Linear Regression
  b=solve(t(tx)%*%tx,tol = 1e-17)%*%(t(tx)%*%ty) # training (estimating b)
  return (b)
}
k = 15 #number of days being tracked at a time
l = 300 #total count of days for simulation
#install.packages("Quandl")
library(Quandl)
rawdata = Quandl("OPEC/ORB")
rawdata$Date <- as.Date(rawdata$Date, "%Y-%m-%d")
startBal = 1000
finalBal = startBal
numshares = 0
todayPrice = 0
for (i in 0:(l-k-1)){ #loop number of times equal to total-k since we already have k values
  mydata = rawdata[(l-k-i):(l-i),] #slice matrix based on count-k-iterations up to count-iterations
  trainx = matrix(mydata$Date,nrow = nrow(mydata), ncol=1) #training data x
  trainX = cbind(trainx, matrix(1,nrow=nrow(trainx),ncol=1)) #training data X # all element of the second column is 1
  trainy = matrix(mydata$Value,nrow = nrow(mydata), ncol=1) #training data y
  
  b = LR1D(trainX, trainy) # estimate vector beta (size is 2)
  
  plot(as.Date(trainX[,1]), trainy, pch=21, bg="black", xlab="Date", ylab="Oil Price") # plot samples
  abline(b[2],b[1]) # draw the linear model (beta)
  predictedPrice = (trainx[1]+1)*b[1]+b[2] #use slope and intercept to predict tomorrows price
  todayPrice = trainy[1] #fetch todays price
  if(predictedPrice/todayPrice < .98 && numshares>0){#we sell if we predict price to go down by 2%
    finalBal=todayPrice+finalBal #sell a share
    numshares=numshares-1
    print("Selling today")
    print(as.Date(trainx[1]))
  }else if(predictedPrice/todayPrice > 1.02 && finalBal >= todayPrice){#we buy if we predict price to go up by 2%
    finalBal=finalBal-todayPrice #buy a share
    numshares=numshares+1
    print("Buying today")
    print(as.Date(trainx[1]))
  }
  Sys.sleep(.05) #pause to see plot
}
finalBal = finalBal+(todayPrice*numshares)
print("Earning Rate:")
print(finalBal/startBal)





