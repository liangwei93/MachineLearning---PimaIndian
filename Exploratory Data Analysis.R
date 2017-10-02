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
dat[, names(biological_data)] <- biological_data


