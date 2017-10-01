# LY: I'm using the in built R data set of Pima Indians Diabetes so diabetes(outcome) is alr of type factor #

dat <- PimaIndiansDiabetes
## Checking classes of various columns in dataset
str(dat)
## Changing the class of Outcome Column into factor
dat$Outcome <- factor(make.names(dat$Outcome))
str(dat)

summary(dat)
## some biological measurements have value 0 in the dataset, and that’s impossible
## setdiff function is used to eliminate such problem
biological_data <- dat[,setdiff(names(dat), c('Outcome', 'Pregnancies'))]
features_miss_num <- apply(biological_data, 2, function(x) sum(x<=0))
features_miss <- names(biological_data)[ features_miss_num > 0]
features_miss_num
## LW : i got no fucking idea whata are the above doing

## LY: 
## Based on wiki, the essential body fat percentage for females (body fat accounts for the skinfold thickness) is 10-13%
## so it's impossible that certain entries in the 'triceps' column is 0.
## Glucose, pressure, insulin and mass values are also impossible to be 0.
## SO basically the Kaggle code above just checking which feature attribute indeed contains impossible entries of 0 then set them to NAs


rows_errors <- apply(biological_data, 1, function(x) sum(x<=0)>1) 
sum(rows_errors)
## These are a lot of rows. It is more than 30% of the dataset:
sum(rows_errors)/nrow(dat)
## those with problems change to NAs

biological_data[biological_data<=0] <- NA
dat[, names(biological_data)] <- biological_data

## Then to deal with the NAs, instead of using the usual method of setting NAs to 0 (which doesnt work for our dataset)
## Use knnImputation (i.e. KNN method to input N.As with the k-nearest-neighbouring value)
library(VIM)
dat_original <- dat
dat[,-9] <- knnImputation(dat[,-9], k=5)
## got stuck  #LY: works fine for me# 

## proportion of the outcome output
prop.table(table(dat$Outcome))

##correlation between numerical variables
correlat <- cor(dat[, setdiff(names(dat), 'Outcome')])
corrplot(correlat)
correlat
