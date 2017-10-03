library(GGally)
library(DMwR)

## loading of data
data <- diabetes
str(data)
## change Outcome columns into data
data$Outcome <- factor(make.names(data$Outcome))
str(data)
summary(data)

## some biological measurements have value 0 in the dataset, and that’s impossible
## setdiff is used to create a new table without Outcome and Pregnancies
biological_data <- data[,setdiff(names(data), c('Outcome', 'Pregnancies'))]
biological_data
## these are the arguments for apply(X, MARGIN, FUN, ...)
## MARGIN is a variable defining how the function is applied: when MARGIN=1, 
## it applies over rows, whereas with MARGIN=2, it works over columns. (in this case we working columns)
features_miss_num <- apply(biological_data, 2, function(x) sum(x<=0))
features_miss <- names(biological_data)[ features_miss_num > 0]
## Show the number of missing values of each column
features_miss_num

## LY: 
## Based on wiki, the essential body fat percentage for females (body fat accounts for the skinfold thickness) is 10-13%
## so it's impossible that certain entries in the 'triceps' column is 0.
## Glucose, pressure, insulin and mass values are also impossible to be 0.
## SO basically the Kaggle code above just checking which feature attribute indeed contains impossible entries of 0 then set them to NAs

rowswitherrors <- apply(biological_data, 1, function(x) sum(x<=0)>1) 
sum(rowswitherrors)
## These are a lot of rows. It is more than 30% of the dataset:
sum(rowswitherrors)/nrow(data)

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

## Exploratory Analysis 
df <- data.frame(data)
df <- knnImputation(df, k=5, meth="weighAvg" )
summary(df)
## for visualising missing data
md.pattern(df)

## proportion of the outcome output
prop.table(table(df$Outcome))

##correlation between numerical variables
correlat <- cor(df[, setdiff(names(df), 'Outcome')])
corrplot(correlat)
correlat
boxplot(df)
## the wide range above shows the need to normalise the data
## below functions are for scaling and checking
scaled.df <- scale(df[,1:8])
colMeans(scaled.df)  
boxplot(scaled.df)

## logistic regression as baseline model for reference to compare 
## with other techniques
set.seed(4510)
dindex <- createDataPartition(df$Outcome, p=0.7, list=FALSE)
train_data <- df[dindex,]
test_data <- df[-dindex,]
table(train_data$Outcome)
## codes for logistic regression
fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

model_glm <- train(Outcome~.,
                   train_data,
                   method="glm",
                   metric="ROC",
                   tuneLength=10,
                   preProcess = c('center', 'scale'),
                   trControl=fitControl)

pred_glm <- predict(model_glm, test_data)
cm_glm <- confusionMatrix(pred_glm, test_data$Outcome, positive="X1")
cm_glm

library(pROC)
library(caTools)
pred_prob_glm <- predict(model_glm, test_data, type="prob")
roc_glm <- roc(test_data$Outcome, pred_prob_glm$X1)
colAUC(pred_prob_glm$X1, test_data$Outcome, plotROC = TRUE)


