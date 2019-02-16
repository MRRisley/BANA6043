# BANA6043: ANOVA and t-tests in R
# Author: Matt Risley

library(data.table)

# dataset -----------------------------------------------------------------


?InsectSprays
#help file


df <- InsectSprays
# store with an easier name


dt <- as.data.table(df)
# create data table for some examples


nrow(df); ncol(df)
# number rows, number columns
# small enough to print to console


colnames(df)
#column names


class(df$count)
class(df$spray)
#variable types


df
#print dataset


df$spray
spray
#first exists; second does not


dt$spray #data table can use data frame syntax
dt[, spray] #this will work with a single variable - returns a vector
dt[, .(spray)] #this is the best way - returns a list
#data table syntax


?attach
attach(df)
spray
#can use attach()


detach(df)
spray
#detach() removes



# summary of 'count' -----------------------------------------------------------------

mean(df$count)
min(df$count)
max(df$count)
median(df$count)
quantile(df$count)
#summary stats


summary(df$count)
#summary function


hist(df$count)
#default histogram


hist(df$count, main="Insect Count", breaks=10)
#change title and number of bins


boxplot(df$count)
#default boxplot


# summary of 'spray' ------------------------------------------------------


unique(df$spray)
length(unique(df$spray))


table(df$spray)
#PROC FREQ analog


dt[, .N, by=spray]
#data table
#.N is data table syntax for count



# bivariate analysis ------------------------------------------------------

?by
by(df$count, df$spray, mean)
by(df$count, df$spray, summary)
#applies a function for a variable across a factor


dt[, mean(count), by=spray] #column is not named
dt[, .(mean=mean(count)), by=spray] 
dt[, .(.N, mean=mean(count)), by=spray] #can return more than one column
#data table syntax


boxplot(count ~ spray, data = df,
        xlab = "type of spray", ylab = "insect count",
        main = "InsectSprays data", col = "lightgray")
#boxplot for each spray


par(mfrow=c(3, 2))
#create panel dimensions for graphical output
# 3 rows by 2 columns

hist(df$count[df$spray=='A'], main="A", freq=T, xlim=c(0, 30))
hist(df$count[df$spray=='B'], main="B", freq=T, xlim=c(0, 30))
hist(df$count[df$spray=='C'], main="C", freq=T, xlim=c(0, 30))
hist(df$count[df$spray=='D'], main="D", freq=T, xlim=c(0, 30))
hist(df$count[df$spray=='E'], main="E", freq=T, xlim=c(0, 30))
hist(df$count[df$spray=='F'], main="F", freq=T, xlim=c(0, 30))

dev.off()



# t-tests --------------------------------------------------------

# https://www.statmethods.net/stats/ttest.html

y1 <- df$count[df$spray=='A']
y2 <- df$count[df$spray=='B']
# OR
y1 <- with(df, count[spray=='A'])
y2 <- with(df, count[spray=='B'])
# OR with dt
y1 <- dt[spray=='A', count]
y2 <- dt[spray=='B', count]


t.test(y1, y2)
# two-tailed of two group means


tt <- t.test(y1, y2)
tt
tt$conf.int
tt$parameter
tt$method


?t.test
#default is that variances are unequal
#probably safe as a default


var.test(y1, y2)
#same variances


t.test(y1, y2, var.equal = T)
#var.equal option




# anova -------------------------------------------------------------------

?aov


aov(df$count ~ df$spray)
# OR 
aov(count ~ spray, data=df)
#note: no p-value


ft <- aov(count ~ spray, data = df)
summary(ft)


plot(ft)


ft2 <- aov(sqrt(count) ~ spray, data = df)
summary(ft2)
plot(ft2)
# has zeros so cannot use log

