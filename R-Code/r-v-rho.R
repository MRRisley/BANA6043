############################################
#create a linear process
############################################

x <- c(1:100) #creates a vector of length 100 from 1 to 100
print(x)

y <- 5 + 3*x #transforms x to y through a linear process

cbind(x, y) #puts x and y into a matrix through "column-binding"

plot(x, y, main='Linear Process') #plots them and adds a title

?cor #help for R's correlation function
cor(x, y) #pearson is default
cor(x, y, method='spearman') #spearman

# pearson and spearman = 1

############################################
#create a monotonic, non-linear process - #1
############################################

x1 <- c(1:100)

y1 <- 5 + 0.5*x1 + 2*x1^2 + 3*x1^3

plot(x1, y1, main='Non-Linear Process #1')

cor(x1, y1)
cor(x1, y1, method='spearman')
#pearson is reduced, but still high
#spearman remains = 1

############################################
#create an obviously non-linear process - #2
############################################
x2 <- c(1:100)

y2 <- 5 + 0.5*x2 + 2*x2^2 + x2^10

plot(x2, y2, main='Non-Linear Process #2')

cor(x2, y2)
cor(x2, y2, method='spearman')
#pearson is pretty low
#spearman remains = 1

############################################
#compare #1 and #2 - scale can be deceiving:
############################################

#NOTE: this part is advanced - not expecting you to learn right now

matplot(cbind(x1, x2), cbind(y1, y2), main='Non-Linear Process #1 and #2', type='l', lty=1, lwd=2)
legend('topleft', leg=c('#1', '#2'), lty=1, col=1:2)
#1 doesn't even register

############################################
#create a stranger monotonic function:
############################################
x3 <- c(1:100)

?seq #help for sequence function
y3 <- seq(from = 0.001, by=0.0005, length=99)
print(y3)
length(y3)

y3 <- c(y3, 5^20) #adding one more value to y3
print(y3)
length(y3)
  
plot(x3, y3)

cor(x3, y3)
cor(x3, y3, method='spearman')
#pearson near zero
#spearman remains = 1

