# BANA 6043: Regression - Dummy Variables
# Week 6 Lecture
# 
# Author: Matt Risley
#
# Date: Feb-2019


# set up ------------------------------------------------------------------

#install.packages('lmtest')
#install.packages('tseries')
library(lmtest)
library(tseries)

df <- InsectSprays

class(df$spray)
#factors can easily be worked into a regression as dummy variables


# regression --------------------------------------------------------------

r <- lm(count ~ spray, data=df)
summary(r)


# test residuals

resid <- r$residuals


plot(resid, main = "Residual by Observation #")
abline(h=0)


mean(resid) #close to zero
lmtest::dwtest(r)
lmtest::bgtest(r, order = 1)
lmtest::bgtest(r, order = 2)


fitted <- r$fitted.values


plot(fitted, resid, main="Residuals vs. Predicted Values")
#larger variance in residuals for larger fitted values

lmtest::bptest(r)
#heteroskedastic

hist(resid)
#pretty normal

tseries::jarque.bera.test(resid)
#normal


# dummy variable interpretation -------------------------------------------

#manually create a single dummy variable

n <- nrow(df)

for (i in 1:n) {
  s1[i] = if (df$spray[i]=='A') 1 else 2
}

s1

r <- lm(count ~ s1, data=df)
summary(r)

new <- data.frame(s1 = c(1, 2))
predict(r, new)

alpha <- as.vector(r$coef[1]) 
beta <- as.vector(r$coef[2])

# by hand
s1 = 1
y1 = alpha+beta #don't need the one

s2 = 2
y2 = alpha+beta*2

y1; y2


# only use 0 and 1
n <- nrow(df)

for (i in 1:n) {
  s1[i] = if (df$spray[i]=='A') 1 else 0
}

s1

r <- lm(count ~ s1, data=df)
summary(r)

alpha <- as.vector(r$coef[1]) 
beta <- as.vector(r$coef[2])

new <- data.frame(s1 = c(1, 0))
predict(r, new)
#same, but:

# by hand
s1 = 1
y1 = alpha+beta #don't need the one

s2 = 0
y2 = alpha #don't need beta 

y1; y2

# so, when we have dummy variables that are 0 or 1:

# intercept represents the "base" where the dummy variable = 0
# when spray <> 1, the expected count = 8.5

# beta represents difference between the two levels
# "spray 1 has 6 more insect counts than other sprays"



# transformation with more normal residuals -------------------------------

r <- lm(count^(1/2) ~ spray, data=df)
summary(r)

# test residuals

resid <- r$residuals

plot(resid, main = "Residual by Observation #")
abline(h=0)

mean(resid) #close to zero
lmtest::dwtest(r)
lmtest::bgtest(r, order = 1)
lmtest::bgtest(r, order = 2)


fitted <- r$fitted.values

plot(fitted, resid, main="Residuals vs. Predicted Values")
#larger variance in residuals for larger fitted values

lmtest::bptest(r)
#homoroskedastic

hist(resid)
#pretty normal

tseries::jarque.bera.test(resid)
#normal


