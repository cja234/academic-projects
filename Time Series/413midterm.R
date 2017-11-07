#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")

#train <- h5read("train.h5", "train")
#str(train)

# Load data.
#dataset <- h5read("train.h5", "train")
#block0_values <- t(dataset$block0_values)
#block1_values <- t(dataset$block1_values)
#colnames(block0_values) <- dataset$block0_items
#colnames(block1_values) <- dataset$block1_items
#dataset <- cbind(block0_values, block1_values)


library(rhdf5)
library(ggplot2)
library(corrplot)
# Adapted from http://pandas.pydata.org/pandas-docs/stable/io.html#io-external-compatibility 
read_hdf <- function(h5path, dataframe_name=NULL) {
  h5File <- H5Fopen(h5path, flags="H5F_ACC_RDONLY")
  listing <- h5ls(h5File)
  
  if (is.null(dataframe_name)) {
    dataframe_name <- listing$name[1]
  }
  
  group_name <- paste0("/", dataframe_name)
  
  # Filter to just the requested dataframe:
  listing <- listing[listing$group == group_name,]
  
  # Find all data nodes, values are stored in *_values and corresponding column
  # titles in *_items
  data_nodes <- grep("_values$", listing$name)
  name_nodes <- grep("_items$", listing$name)
  data_paths = paste(listing$group[data_nodes], listing$name[data_nodes], sep = "/")
  name_paths = paste(listing$group[name_nodes], listing$name[name_nodes], sep = "/")
  columns = list()
  for (idx in seq(data_paths)) {
    # NOTE: matrices returned by h5read have to be transposed to to obtain
    # required Fortran order!
    data <- data.frame(t(h5read(h5File, data_paths[idx])))
    names <- t(h5read(h5File, name_paths[idx]))
    entry <- data.frame(data)
    colnames(entry) <- names
    columns <- append(columns, entry)
  }
  
  data <- data.frame(columns)
  
  # If "axis0" is specified, we can return the dataframe columns in the original order:
  if ("axis0" %in% listing$name) {
    orig_col_order <- h5read(h5File, paste0(group_name, "/axis0"))
    data <- data[orig_col_order]
  }
  
  H5Fclose(h5File)
  return(data)
}

train <- read_hdf("train.h5")

library(plyr)
mean = ddply(data.frame(train), .(timestamp), summarise, mean_target = mean(y))
qplot(timestamp, mean_target, data = mean, geom = "line",ylim = c(-0.01,0.01))

#Assuming Joint distribution to be normal
#Correlation between derived variables
#Derived_0 and derived_4 are highly correlated with a value -0.96
correlations <- cor(train[,c(3:7)],use="pairwise.complete.obs",method="pearson")
corrplot(correlations, method="color", order="hclust")

#Checking the correlation between derived_0 and derived_4 using scatter plot
qplot(derived_0, derived_4, data = data.frame(train), geom = c("point", "smooth"))

#Similarly checking for fundamental and technical features
correlations <- cor(train[,c(8:70)],use="pairwise.complete.obs",method="pearson")
corrplot(correlations, method="color", order="hclust",numbers=TRUE)

##(fundamental_5, fundamendal_16) + (fundamendal_7, fundamental_36) + (fundamental_32,fundamental_13) + 
#(fundamental_31,fundamental_13)+ (fundamental_13,fundamental_46)+ (fundamental_50,fundamental_23)

#All the above have correlation coefficient >= 0.99

#Looking into scatter plots
qplot(fundamental_5, fundamental_16, data = data.frame(train), geom = c("point", "smooth"))
qplot(fundamental_7, fundamental_36, data = data.frame(train), geom = c("point", "smooth"))
#Created by mistake # But actualy would be exciting to see other similar patterns
#Complimentary
qplot(fundamental_32, fundamental_13, data = data.frame(train), geom = c("point", "smooth"))
qplot(fundamental_13, fundamental_31, data = data.frame(train), geom = c("point", "smooth"))
qplot(fundamental_13, fundamental_46, data = data.frame(train), geom = c("point", "smooth"))
qplot(fundamental_32, fundamental_12, data = data.frame(train), geom = c("point", "smooth"))

#now for
correlations <- cor(train[,c(71:110)],use="pairwise.complete.obs",method = "pearson")
corrplot(correlations, method="color", order="hclust")

#Function for finding missing values in each column
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(data.frame(train))

#Distribution of features
par(mfrow=c(3, 3))
colnames <- dimnames(data.frame(train))[[2]]
for (i in 2:110) {
  hist(train[,i], main=colnames[i], probability=TRUE, col="blue", border="white")
}




for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}

propmiss(data.frame(train))

str(train)
train.data <- sample(length(train$id), 10000, replace = FALSE)

library(randomForest)
rf.fit <- randomForest(y ~ . -id, data = train, subset = train.data, mtry = 11, importance = T)
rf.fit
importance(rf.fit)
varImpPlot(rf.fit)

slr.fit <- lm(y ~ technical_21, data = train)
summary(slr.fit)

mlr.fit <- lm(y ~ technical_21 + technical_30, data = train)
summary(mlr.fit)











