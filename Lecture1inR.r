###Lecture 1###

#Set working directory
setwd("D:/R-Statistics") 

#Load package
library(quantmod)

# 1. start with Apple Stocks

#Download daily prices of Apple
getSymbols("AAPL")

#Dimension: Size of downloaded data.
dim(AAPL)

# See the first 6 rows of the data
head(AAPL)

# See the last 6 rows of the data
tail(AAPL)

#Plot the daily price and volume
chartSeries(AAPL, theme="white")

#Plot just the closing prices
chartSeries(AAPL[,4], theme="white")

#plot just the closing prices
chartSeries(AAPL[,4],theme="white",major.tick="years")

#Not shown giving the same plot with black background.
chartSeries(AAPL)

#select a specific period to plot
chartSeries(AAPL, subset='2019::2021-1',theme="white", major.tick="quarters")

#select a specific period to download data
getSymbols("AAPL", from="2005-01-02", to="2023-08-17")

#Head
head(AAPL)

# 2. download some others mentioned in the book

getSymbols("UNRATE",src="FRED")#Download unemployment rates from FRED.
head(UNRATE)
chartSeries(UNRATE,theme="white")  # Plot monthly unemployment rates
getSymbols("INTC",src="yahoo")  # Download data from Google.
head(INTC)
getSymbols("^TNX") # Download CBOE 10-year Treasures Notes
head(TNX)  
chartSeries(TNX,theme="white",TA=NULL) # Obtain plot without volume.

# 3. load the book data from the text file 

#load the package
library(fBasics)

# Load text data with names.
da=read.table('d-ibm-0110.txt', header=T)

#See first 6 rows
head(da)

#Dimension of the data object "da"
dim(da)

# Load csv data with names.
da <- read.csv("d-vix0411.csv",header=T)


# 4. calculate and plot the returns from the price data

###### simple returns (using closing prices)

#Just 1 day
AAPL.ret1=periodReturn(AAPL,period='daily',subset='2023-08-15') 


#Series
AAPL.ret2=periodReturn(AAPL,period='daily',subset='2023-01-01::2023-02-01')
chartSeries(AAPL.ret2,theme="white")

# multiperiod

#Month
APPL.retM1=periodReturn(AAPL,period='monthly', subset='2023-01-01::2023-01-31')


#Monthly return within a year
AAPL.retM2=periodReturn(AAPL, period='monthly', subset='2022-01-01::2023-12-31')
chartSeries(AAPL.retM2,theme="white")

# yearly
APPL.retANN =periodReturn(AAPL,period='yearly',subset='2022-12-31')

# quarterly annualized
APPL.retQuart2ANN=(1+periodReturn(AAPL,period='quarterly',subset='2022-06-30'))^4-1

# series of annualized quarterly returns
APPL.retQUart2ANN1=(1+periodReturn(AAPL,period='quarterly'))^4-1

# average (arithmetic) return over a period

APPL.retMAV=mean(AAPL.retM2)

#### log returns (using closing prices)

AAPL.logret=periodReturn(AAPL,period='daily',subset='2023-02-01',type='log')

# series
AAPL.logret1= periodReturn(AAPL,period='daily', subset='2023-01-01::2023-08-16',type='log')
                           
# multiperiod
AAPL.logretM1=periodReturn(AAPL,period='monthly',subset='2023-01-01::2023-02-01',type='log')
AAPL.logretM2=periodReturn(AAPL,period='monthly',subset='2022-01-01::2022-12-31',type='log')

#Yearly
AAPL.logretANN= periodReturn(AAPL,period='yearly',subset='2022-12-31',type='log')

#Quarterly period
AAPL.logretQuart2ANN1 = (1+periodReturn(AAPL,period='quarterly',subset='2023-06-30',type='log'))^4-1
AAPL.logretQuart2ANN2 = (1+periodReturn(AAPL,period='quarterly',type='log'))^4-1

#Average log return over a period (geometric average of simple return)
AAPL.logretMAV=mean(AAPL.logretM2)

#alternative way to compute the log returns
AAPL.logret3=diff(log(AAPL$AAPL.Adjusted))
chartSeries(AAPL.logret3,theme="white")

# 5. Create simple portfolio

#load data for Citibank
getSymbols("C")

#see dimension
dim(C)

#Return
AAPL.ret=periodReturn(AAPL,period='daily',subset='2022-01-01::2022-12-31')
C.ret=periodReturn(C,period='daily',subset='2022-01-01::2022-12-31')

#Create simple portfolio with 50% weights
PFret = 0.5*AAPL.ret + 0.5*C.ret

#Repeat all with monthly return
AAPL.retM=periodReturn(AAPL,period='monthly', subset='2022-01-01::2022-12-31')
C.retM=periodReturn(C,period='monthly',subset='2022-01-01::2022-12-31')

PFretM = 0.5*AAPL.retM + 0.5*C.retM

#6. collect and plot other data in the book
getSymbols("^TNX", from='2007-01-03',to='2023-08-18')
TNX.return=diff(TNX$TNX.Adjusted) #Compute changes
chartSeries(TNX.return,theme="white")
getSymbols("DEXUSEU",src="FRED") #Obtain exchange rates from FRED
head(DEXUSEU)
tail(DEXUSEU)
DEXUSEU.rtn=diff(log(DEXUSEU$DEXUSEU))
chartSeries(DEXUSEU,theme='white')                 
chartSeries(DEXUSEU.rtn,theme='white')

library(fBasics) # Load the package
da=read.table("d-mmm-0111.txt",header=T) #load the data
#Header=T means that the first row of data file contains names.
#default is no names.
head(da) #show the first 6 rows of data
mmm=da[,2] #obtain 3m simple returns


## 7. analyze distributional properties

# 7.1. look at the moments

mmm= periodReturn(AAPL,period='monthly',type='log') #Monthly returns of apple

basicStats(mmm) #Compute summary statistics
mean(mmm) #Mean
var(mmm) #Variance
stdev(mmm) #standard deviation
t.test(mmm) #testing mean return = 0
SK=skewness(mmm)
T=length(mmm) #sample size
t3=SK/sqrt(6/T) #skewness test
pval_S=2*(1-pnorm(abs(t3))) #Compute p-value
Kk = kurtosis(mmm)
t4=SK/sqrt(24/T)
pval_K=2*(1-pnorm(abs(t4))) #compute p-value
normalTest(mmm,method='jb') #JB Test

# 7.2. look at the density

range(mmm) #range of returns
hist(mmm,nclass=30) #Histogram
d1=density(mmm) #obtain density estimate
x=seq(-.4,.4,.001) #create sequence of x with increment of 0.001
plot(d1$x,d1$y,xlab='rtn',ylab='density',type='l') #plot the density

y1=dnorm(x,mean(mmm),stdev(mmm)) #create artificial data from normal distribution
#with mean and variance equal to sample values of Apple returns
lines(x,y1,lty=2) #add density of normal r.v.

getSymbols("AAPL", from='2007-01-01')
getSymbols("IBM")
# 7.3. bivariate relations

# linear relations

aapl.M =periodReturn(AAPL,period='monthly',type='log') # monthly returns of Apple

IBM.M =periodReturn(IBM,period='monthly',type='log') # monthly returns of IBM

rt=cbind(aapl.M,IBM.M)

m1=apply(rt,2,mean) 
v1=cov(rt)
v1
cor(aapl.M,IBM.M)

mreg=lm(aapl.M~IBM.M) #fit for linear regression
summary(mreg)

xx=cbind(as.numeric(aapl.M),as.numeric(IBM.M)) #to use standard plots move to numeric values
plot(xx[,1],xx[,2],xlab='apple',ylab='IBM',cex=0.8)#Scatter Plot
abline(0.008,.807)#add the regression line estimate

# estimate conditional density
install.packages('np')
library(np)
colnmm=(c('aapl','IBM'))
datas= as.data.frame(xx)
names(datas)=colnmm
View(datas)#check if all is ok

# for fun estimate nonparametric regression
model.np <- npreg(aapl~IBM, regtype = "ll", bwmethod = "cv.aic", gradients = TRUE,data=datas)
npsigtest(model.np) #test the significance of the regressor using nonparametric methods

# estimate conditional PDF
bw <- npcdensbw(xdat=datas[,2],ydat=datas[,1],tol=.1,ftol=.1)# find the optimal bandwidth
fhat <- npcdens(bws=bw) #obtain the model estimate

# plot the density
plot(fhat,view="fixed", main="",xlab="IBM",ylab='apple',theta =120, phi=30)

# estimate conditional CDF
bwdist <-npcdistbw(xdat=datas[,2], ydat=datas[,1], tol=.1, ftol=.1)
Fhat <- npcdist(bws=bwdist)
plot(Fhat, view = "fixed", main = "", xlab='citi',ylab='apple', theta = 120, phi = 30)
