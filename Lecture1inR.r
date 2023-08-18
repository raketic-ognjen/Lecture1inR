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
