#-------Info-------#
# Matt Risley
# University of Cincinnati
#
# Statistical Computing 
#
# INTRO TO TIME SERIES FOR FINANCIAL APPLICATIONS
#------------------#


# load packages -----------------------------------------------------------

# install the package if you don't have it
# install.packages('quantmod')
# quant mod github: https://github.com/joshuaulrich/quantmod

# must load the package at start-up
library(quantmod)


# acquire stock data ------------------------------------------------------------

# getSymbols()is a function from the quantmod R package
# it allows you to get data from yahoo, FRED, etc.
#
# return.class = 'ts' is an optional argument that returns
# the stock quote as a 'ts' (time series) object
getSymbols("AAPL", src = "yahoo")

head(AAPL)

#built-in chart from quantmod package
chartSeries(AAPL)

plot(AAPL$AAPL.Close, main="AAPL")


# create random walk ------------------------------------------------------

# how many days of stock information are there?
n <- length(AAPL$AAPL.Close)
# n <- nrow(AAPL)

# what is AAPL's standard deviation?
aapl_sd <- sd(AAPL$AAPL.Close)
# n <- nrow(AAPL)

# create vector of same length
rw <- rep(0, n)

# create random component
set.seed <- 3
epsilon <- rnorm(n, mean=0, sd=aapl_sd)
plot(epsilon, type='l', main="white noise with mean = 0 and sd=54.3")

# set first value of random walk = first AAPL stock value
rw[1] <- AAPL$AAPL.Close[1]
head(rw); head(AAPL$AAPL.Close)

# loop to generate other values
for (i in 2:n) {
  rw[i] = rw[i-1] + epsilon[i]
}

plot(rw, type='l')

#what is the correlation
cor(AAPL$AAPL.Close, rw)

#panel with 1 row, 2 cols
par(mfrow=c(1, 2))

plot(as.vector(AAPL$AAPL.Close), type='l', col='dodgerblue2', main='AAPL',
     ylab='price')

plot(rw, type='l', col='coral', main='Random Walk',
     ylab='price')

# transform into changes --------------------------------------------------

aapl_diff <- diff(as.vector(AAPL$AAPL.Close))
plot(aapl_diff, type='l', main="Change in AAPL", xlab="change in price")

rw_diff <- diff(rw)
plot(rw_diff, type='l', main="Change in Random Walk", xlab="change in price")

# no relationship
cor(aapl_diff, rw_diff)

# moral is that you should not conduct analyses with time series in 
# levels when there is a trend!!!!


