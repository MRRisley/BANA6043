# BANA 6043: Regression - Interpretation of Log Coefficients
# Week 6 Lecture
# 
# Author: Matt Risley
#
# Date: Feb-2019


# set-up ------------------------------------------------------------------

library(ggplot2)
# to load diamonds data


?lm
# lm function is primary function for linear models




# ln(y) = x -----------------------------------------------------------

################
# CASE 1
################

price <- log(diamonds$price)
carat <- diamonds$carat
#transform


cor(price, carat)
# high linear correlation 0.92
cor(price, carat, method="spearman")
# higher monotonic correlation 0.96


r <- lm(price ~ carat)
summary(r)
# the basic regression in r


r$coefficients
alpha <- r$coef[1]
beta <- r$coef[2]
#coefficients from the model



################
# interpret with cheatsheet
################

(exp(beta*0.1)-1)*100 #21.8%
#change in x of 0.1 (1/10th of a carat) with exact

beta*0.1*100 #19.7%
#change in x of 0.1 (1/10th of a carat) with approximate

noquote(paste0(round(beta*0.1*100, 1), "%"))
#prettify


prettify <- function (x) {noquote(paste0(round(x, 1), "%"))}
prettify(beta*0.1*100)
#turn into a function



################
# checks
################


x1 <- 1
x2 <- 1.1 #0.1 carat increase
new <- data.frame(carat = c(x1, x2))
yhat <- predict(r, new)
yhat
exp(yhat)

exp(yhat[2])-exp(yhat[1]) # $780
prettify((exp(yhat[2])/exp(yhat[1])-1)*100) # 21.8%
# predict function


prettify <- function (x, mult=T) {
  if (mult) x <- x*100
  noquote(paste0(round(x, 1), "%"))
  }

prettify(exp(yhat[2])/exp(yhat[1])-1)
prettify((exp(yhat[2])/exp(yhat[1])-1)*100, mult=F)
prettify((exp(yhat[2])/exp(yhat[1])-1)*100, F)
#simplify prettify


y1 <- exp(alpha+beta*x1)
y2 <- exp(alpha+beta*x2)

y2-y1 # $780
prettify(y2/y1-1) # 21.8%
# by hand




# y = ln(x) -----------------------------------------------------------
 
################
# CASE 2
################

price <- diamonds$price
carat <- log(diamonds$carat)
#transform


cor(price, carat)
# strong linear correlation 0.86
cor(price, carat, method="spearman")
# higher monotonic correlation 0.96


r <- lm(price ~ carat)
summary(r)

alpha <- r$coef[1]
beta <- r$coef[2]



################
# interpret with cheatsheet
################

beta*log(1.01) # $58
#change in x of 1% with exact

beta/100 # $58
#change in x of 1% with approximate

# always $58 for a 1% increase in carat:
# 1 carat to 1.01 
# 10 carat to 10.1


beta*log(1.1) # $556
#change in x of 10% with exact

beta/100*10 # $583
#change in x of 1% with approximate

# always $556 for a 10% increase in carat:
# 1 carat to 1.1 
# 10 carat to 11 (unlikely)



################
# checks
################

x1 <- 1
x2 <- 1.1 # 10% carat increase
new <- data.frame(carat = c(log(x1), log(x2)))
yhat <- predict(r, new)
yhat

yhat[2]-yhat[1] # $556
# predict function


y1 <- alpha+beta*log(x1)
y2 <- alpha+beta*log(x2)

y2-y1 # $556
# by hand




# ln(y) = ln(x) -----------------------------------------------------------

################
# CASE 3
################

price <- log(diamonds$price)
carat <- log(diamonds$carat)
#transform


cor(price, carat)
# high linear correlation 0.97
cor(price, carat, method="spearman")
# equivalent monotonic correlation


r <- lm(price ~ carat)
summary(r)

alpha <- r$coef[1]
beta <- r$coef[2]


###
# interpret with cheatsheet
###

#change in x of 1% with exact
prettify(1.01^beta - 1) #1.7%

#change in x of 1% with approximate
beta # 1.7%

#change in x of 10% with exact
prettify(1.1^beta - 1) # 17.3%

#change in x of 10% with approximate
beta*10 # 16.6%


###
# checks
###

# predict function
x1 <- 2
x2 <- 2.02 # 1% carat increase
new <- data.frame(carat = c(log(x1), log(x2)))
yhat <- predict(r, new)
exp(yhat)

exp(yhat[2])-exp(yhat[1]) # $250 
prettify(exp(yhat[2])/exp(yhat[1])-1) # 1.7%


# by hand
y1 <- exp(alpha+beta*log(x1))
y2 <- exp(alpha+beta*log(x2))

prettify(y2/y1-1) # 1.7%
