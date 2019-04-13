#-------Info-------#
# Matt Risley
# University of Cincinnati
#
# Statistical Computing 
#
# INTRO TO LOGS FOR FINANCE
#------------------#


# load packages -----------------------------------------------------------

library(quantmod)
library(ggplot2)

# pull data ---------------------------------------------------------------

getSymbols("GDPC1", src = "FRED")
# real GDP
# https://fred.stlouisfed.org/series/GDPC1

# plot real GDP --------------------------------------------------------------------

plot(GDPC1)

l_gdp <- log(GDPC1)

plot(l_gdp)
# the logarithm tends to "linearize" data

# plot diamonds data ------------------------------------------------------

par(mfrow=c(1, 2))
#panel with 1 row, 2 columns in plot window

plot(diamonds$carat, diamonds$price, col='dodgerblue', main='Levels')
abline(lm(price~carat, data=diamonds), col='dodgerblue4', lwd=4)
# adds a trend line with lm() function

l_carat <- log(diamonds$carat)
l_price <- log(diamonds$price)

plot(l_carat, l_price, col='dodgerblue', main='Logs')
abline(lm(l_price~l_carat), col='dodgerblue4', lwd=4)

cor(diamonds$carat, diamonds$price)
cor(l_carat, l_price)
# logs have stronger linear relationship

hist(diamonds$price, main='Levels', col='dodgerblue2')
hist(l_price, main='Logs', col='coral')
# logs will also "normalize" most skewed data

dev.off()
# turns of par options (the panel)

# time series with logs ---------------------------------------------------

ld_gdp <- diff(l_gdp)
# log difference

gr_gdp <- rep(NA, length(ld_gdp))
# create empty vector to populate with growth rates

vec_gdp <- as.vector(GDPC1)
# transform GDPC1 into a vector

for (i in 2:length(gr_gdp)) {
  gr_gdp[i] <- (vec_gdp[i] - vec_gdp[i-1])/vec_gdp[i-1]
}
# loop to populate

cbind(ld_gdp, gr_gdp)
# put in matrix

matplot(cbind(ld_gdp, gr_gdp), type='l', col=c('dodgerblue2', 'coral'))
# plot them together
# virtually no difference

x <- ld_gdp-gr_gdp

plot(as.vector(x)*100, type='l', ylab="%")
