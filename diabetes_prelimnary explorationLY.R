# LY: I'm using the in built R data set of Pima Indians Diabetes so diabetes(outcome) is alr of type factor #
library(mlbench)
library(VIM)
library(dplyr)
library(caret)
library(ggplot2)
library(glmnet)
library(rpart)
library(partykit)
library(randomForest)
library(DMwR)
library(corrplot)
library(mice)


dat <- PimaIndiansDiabetes
## Checking classes of various columns in dataset
str(dat)
attach(dat)
## Changing the class of Outcome Column into factor
dat$Outcome <- factor(make.names(dat$Outcome))
str(dat)

summary(dat)
## some biological measurements have value 0 in the dataset, and that’s impossible
## setdiff function is used to eliminate such problem
biological_data1 <- dat[,setdiff(names(dat), c('Outcome', 'Pregnancies'))]
biological_data1
## these are the arguments for apply(X, MARGIN, FUN, ...)
## MARGIN is a variable defining how the function is applied: when MARGIN=1, 
## it applies over rows, whereas with MARGIN=2, it works over columns. (in this case we working columns)
features_miss_num1 <- apply(biological_data1, 2, function(x) sum(x<=0))
features_miss1 <- names(biological_data1)[ features_miss_num1 > 0]
## Show the number of missing values of each column
features_miss_num1

## LY: 
## Based on wiki, the essential body fat percentage for females (body fat accounts for the skinfold thickness) is 10-13%
## so it's impossible that certain entries in the 'triceps' column is 0.
## Glucose, pressure, insulin and mass values are also impossible to be 0.
## SO basically the Kaggle code above just checking which feature attribute indeed contains impossible entries of 0 then set them to NAs


rows_errors1 <- apply(biological_data1, 1, function(x) sum(x<=0)>1) 
sum(rows_errors)
## These are a lot of rows. It is more than 30% of the dataset:
sum(rows_errors)/nrow(dat)
## those with problems change to NAs

biological_data[biological_data<=0] <- NA
data[, names(biological_data)] <- biological_data
## saving a copy of all the changes up to now so we can refer back if need be
original_data <- data

## data visualisation of missing data, as we can see close to 50% of insulin data 
## and 30% of skinthickness data are missing
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(data), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()
marginplot(data[c(1,2)])

## Then to deal with the NAs, instead of using the usual method of setting NAs to 0 (which doesnt work for our dataset)
## Use knnImputation (i.e. KNN method to input N.As with the k-nearest-neighbouring value)

dat[,-9] <- knnImputation(dat[,-9], k=5)
## got stuck  #LY: works fine for me# 
md.pattern(data)

## proportion of the outcome output
prop.table(table(dat$Outcome))

##correlation between numerical variables
correlat <- cor(dat[, setdiff(names(dat), 'Outcome')])
corrplot(correlat)
correlat
