#########################
#         HW 5          # 
#########################


# SDGB 7844: Statistical Methods and Computation I
# Sumi Choudhury

########### Part 1 ###########
rm(list=ls())
#Upload the data in \asset data.txt" into R and call the data frame data.x. 
data.x <- read.csv("asset_data.txt")

View(data.x)

#Next, use the command:data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")
data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")

#Extract only the observations where the federal funds rate is available
#(so you are left with weekly data);
RedData <-na.omit(data.x)

View(RedData)

#What is the start date and end date of this reduced data set?
min(RedData$date)
max(RedData$date)

#Graph the federal funds interest rate as a time series
plot(RedData$date, RedData$fed.rate, type="l", xlab= "Years", ylab= "Fed Rate")

########### Part 2 ###########

#Make two separate data frames; (a) the training set should contain all observations
#before 2014 and (b) the test set should contain all observations in 2014
Train_Set = subset(RedData,date<=as.Date("2013-12-31"))
Test_Set = subset(RedData,date>=as.Date("2014-01-01") & date<=as.Date("2014-12-31"))

#How many observations are in each subset?
nrow(Train_Set)
nrow(Test_Set)
View(Train_Set)
View(Test_Set)

########### Part 3 ###########

#The federal funds interest rate is in percent form so convert it to decimal
head(Train_Set)
Train_Set$fed.rate = Train_Set$fed.rate/100
head(Train_Set)


#Then, for the S&P 500 and long term treasury bonds ETF assets, compute the returns
Train_Set=Train_Set[order(as.Date(Train_Set$date, format="%Y-%m-%d")),]
Train_Set["Return_Spy"]<-0
Train_Set["Return_tlt"]<-0
for (i in 2:570) {
  Train_Set$Return_Spy[i]=(Train_Set$close.spy[i]-Train_Set$close.spy[i-1])/Train_Set$close.spy[i-1]
}
for (i in 2:570) {
  Train_Set$Return_tlt[i]=(Train_Set$close.tlt[i]-Train_Set$close.tlt[i-1])/Train_Set$close.tlt[i-1]
  }

#For both assets, construct time series plots of the returns (separate plots).
plot(Train_Set$date, Train_Set$Return_Spy, type="l", xlab= "Years", ylab= "Return_SPY", ylim = c(-0.16,0.1))
abline(h=0, lty = 3, col ="firebrick", lwd = 2)
plot(Train_Set$date, Train_Set$Return_tlt, type="l", xlab= "Years", ylab= "Return_TLT",ylim = c(-0.16,0.1))
abline(h=0, lty = 3, col ="firebrick", lwd = 2)

########### Part 4 ###########

#Construct two normal quantile plots, one for training set returns of each asset.

# normal quantile plot - Return of SPY
qqnorm(Train_Set$Return_Spy, pch=20, las=TRUE, main = "QQ PLot of SPY")
qqline(Train_Set$Return_Spy, col=c("firebrick"))

####
# normal quantile plot - Return of ETF
qqnorm(Train_Set$Return_tlt, pch=20, las=TRUE, main = "QQ PLot of TLT")
qqline(Train_Set$Return_tlt, col=c("firebrick"))

########### Part 5 ###########
#Compute the correlation between the S&P500 and long term treasury bond returns
#in the training set and interpret it
cor(Train_Set$Return_Spy,Train_Set$Return_tlt)
plot(Train_Set$Return_Spy,Train_Set$Return_tlt, las = T, main = "Return SPY vs. Return TLT", 
     xlab ="Return SPY", ylab ="Return TLT", pch =20, xlim= c(-0.16,0.1), ylim= c(-0.16,0.1))
abline(lsfit(Train_Set$Return_Spy,Train_Set$Return_tlt), col = "firebrick",lwd = 1.8)

#Now, we will compute a rolling-window correlation as follows: compute the 
#correlation between the two asset returns only using the first 24 weeks of data
Train_Set["Rolling_Corr"]<-0
for (i in 2:(546)) {
  Train_Set$Rolling_Corr[i+23]=cor(Train_Set$Return_Spy[i:(i+23)], Train_Set$Return_tlt[i:(i+23)])
}

#make a time series plot of the rolling-window correlation with each point plotted on the last day of the window. Add a horizontal, dotted, gray line at 0 to your plot
plot(Train_Set$date[25:570], Train_Set$Rolling_Corr[25:570], type="l", xlab= "Years", ylab= "Rolling Correlation")
abline(0,0,col=c("dark gray"),lty=3)

########### Part 6 ###########
Train_Set["Et_SPY"]=0
Train_Set["Gt_SPY"]=100
Train_Set["Et_tlt"]=0
Train_Set["Gt_tlt"]=100
for (i in 2:570) {
  #Step 1
  Train_Set$Et_SPY[i] = Train_Set$Return_Spy[i]-Train_Set$fed.rate[i-1]/52
  #Step 2
  Train_Set$Gt_SPY[i] = Train_Set$Gt_SPY[i-1]*(1+Train_Set$Et_SPY[i])
  Train_Set$Et_tlt[i] = Train_Set$Return_tlt[i]-Train_Set$fed.rate[i-1]/52
  Train_Set$Gt_tlt[i] = Train_Set$Gt_tlt[i-1]*(1+Train_Set$Et_tlt[i])
}

#step 3 
n=(570-1)/52

#step 4
CAGR_SPY = (Train_Set$Gt_SPY[570]/Train_Set$Gt_SPY[1])^(1/n)-1
CAGR_tlt = (Train_Set$Gt_tlt[570]/Train_Set$Gt_tlt[1])^(1/n)-1

#step 5
V_SPY = sqrt(52)*sd(Train_Set$Et_SPY[2:570])
V_tlt = sqrt(52)*sd(Train_Set$Et_tlt[2:570])

#step 6
SR_SPY = CAGR_SPY/V_SPY
SR_tlt = CAGR_tlt/V_tlt
SR_SPY
SR_tlt

########### Part 7 ###########

#Write a function which takes the following inputs:......
Sharpe_Ratio<-function(x,r_Asset1=Train_Set$Return_Spy, r_Asset2=Train_Set$Return_tlt, fedRate=Train_Set$fed.rate)
{
  SR=vector(mode="numeric",length = length(x))
  for (j in 1:length(x)) {
    Rate=vector(mode = "numeric",length = length(r_Asset1))
    e=vector(mode = "numeric", length = length(r_Asset1))
    g=vector(mode = "numeric", length = length(r_Asset1))
    g[1]=100
    for (i in 2:length(r_Asset1)) 
      {
      Rate[i]= x[j]*(r_Asset1[i])+(1-x[j])*r_Asset2[i]
      e[i]=Rate[i]-fedRate[i-1]/52
      g[i] = g[i-1]*(1+e[i])
    }
    n=(length(r_Asset1)-1)/52
    CAGR=(g[length(r_Asset1)]/g[1])^(1/n)-1
    V=sqrt(52)*sd(e[2:length(r_Asset1)])
    SR[j]=CAGR/V
  }
  return(SR)
}

#Use the curve() function to plot the Sharpe ratio (y-axiz) for weights between 0 and 1
curve(Sharpe_Ratio(x,Train_Set$Return_Spy,Train_Set$Return_tlt,Train_Set$fed.rate),from = 0, to = 1,xlab = "Weight",ylab = "Sharpe Ratios")

########### Part 8 ###########

#Using the training set, use optimize() to determine the optimum weight for each asset using the function you wrote in question 7;
x_max=optimise(Sharpe_Ratio,lower = 0, upper = 1,maximum = TRUE)
x_max

#What is the Sharpe ratio of the overall portfolio
Sharpe_Ratio(x_max$maximum,Train_Set$Return_Spy,Train_Set$Return_tlt,Train_Set$fed.rate)

########### Part 9 ###########

#In your test set, convert the federal funds interest rate from percent to decimal
#form and compute the returns series for each of the three assets
Test_Set$fed.rate=Test_Set$fed.rate/100

Test_Set=Test_Set[order(as.Date(Test_Set$date, format="%Y-%m-%d")),]
Test_Set["Return_Spy"]<-0
Test_Set["Return_tlt"]<-0

#Next, compute the excess returns index for each asset in the test set (as outlined in question 6)
for (i in 2:43) {
  Test_Set$Return_Spy[i]=(Test_Set$close.spy[i]-Test_Set$close.spy[i-1])/Test_Set$close.spy[i-1]
}
for (i in 2:43) {
  Test_Set$Return_tlt[i]=(Test_Set$close.tlt[i]-Test_Set$close.tlt[i-1])/Test_Set$close.tlt[i-1]
}

Test_Set["Et_SPY"]=0
Test_Set["Et_tlt"]=0
Test_Set["Rate_Portfolio"]=0
Test_Set["Et_Portfolio"]=0
for (i in 2:43) 
  {
  Test_Set$Et_SPY[i] = Test_Set$Return_Spy[i]-Test_Set$fed.rate[i-1]/52
  Test_Set$Et_tlt[i] = Test_Set$Return_tlt[i]-Test_Set$fed.rate[i-1]/52
}

for (i in 2:43) {
  Test_Set$Rate_Portfolio[i]=x_max$maximum*Test_Set$Return_Spy[i]+(1-x_max$maximum)*Test_Set$Return_tlt[i]
  Test_Set$Et_Portfolio[i]=Test_Set$Rate_Portfolio[i]-Test_Set$fed.rate[i-1]/52
  }

#Plot the excess returns index all on the same time series plot and include a
#legend so you can differentiate between the three series
#Add a dotted, horizontal, gray line at y = 100.
plot(Test_Set$date, Test_Set$Et_SPY, type="l", xlab= "Month (2014)", ylab= "Excess Returns",  ylim = c(-0.05,0.1))
lines(Test_Set$date,Test_Set$Et_tlt,type = "l",col=c("red"))
lines(Test_Set$date,Test_Set$Et_Portfolio,type = "l",col = c("blue"))
abline(0,0,col=c("dark gray"),lty = 2)
legend("topright",c("SPY","TLT","Portfolio"), col = c("black","red","blue"), lty = c(1,1))

########### Part 10 ###########

# how much would you have at the end of the test set period for each asset in addition to the risk-free interest rate? Did your portfolio perform well in the test set?
print(Test_Set$Et_SPY[43]*100)
print(Test_Set$Et_tlt[43]*100)
print(Test_Set$Et_Portfolio[43]*100)

