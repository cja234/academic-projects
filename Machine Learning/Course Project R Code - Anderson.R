# PREDICT 422 Practical Machine Learning
# Course Project - R Script File
# March 10, 2017
# Chris Anderson

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

##### DATA EXPLORATION ###############################################################################

# load and check the data
charity <- read.csv(file.choose()) # load the "charity.csv" file
str(charity)
head(charity) 
tail(charity)

# isolate training set for data exploration
data.train <- charity[charity$part=="train",]

# function for finding missing values in each column
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), 
                                                                        propmiss=sum(is.na(x))/length(x)))
propmiss(data.train) # no missing observations
propmiss(charity) # only missing the target variables in the test set

# descriptive statistics for quantitative variables
library(gridExtra)
grid.table(summary(data.train[,c(11:21,23)]))

# examine correlations and distributions of quantitative predictors
library(psych)
pairs.panels(data.train[,c(11:21,23)], cex = 1.25, cex.labels = 1.75)

# possible outliers in plow, tgif, lgif, rgif, and agif
par(mfrow = c(1,5))
boxplot(data.train$plow, main="plow")
abline(h = quantile(data.train$plow, 0.999), col="red")
abline(h = quantile(data.train$plow, 0.99), col="blue")
boxplot(data.train$tgif, main="tgif")
abline(h = quantile(data.train$tgif, 0.999), col="red")
abline(h = quantile(data.train$tgif, 0.99), col="blue")
boxplot(data.train$lgif, main="lgif")
abline(h = quantile(data.train$lgif, 0.999), col="red")
abline(h = quantile(data.train$lgif, 0.99), col="blue")
boxplot(data.train$rgif, main="rgif")
abline(h = quantile(data.train$rgif, 0.999), col="red")
abline(h = quantile(data.train$rgif, 0.99), col="blue")
boxplot(data.train$agif, main="agif")
abline(h = quantile(data.train$agif, 0.999), col="red")
abline(h = quantile(data.train$agif, 0.99), col="blue")

# trim outliers
data.train$plow[data.train$plow > quantile(data.train$plow, 0.999)] <- quantile(data.train$plow, 0.999)
data.train$tgif[data.train$tgif > quantile(data.train$tgif, 0.999)] <- quantile(data.train$tgif, 0.999)
data.train$lgif[data.train$lgif > quantile(data.train$lgif, 0.999)] <- quantile(data.train$lgif, 0.999)
data.train$rgif[data.train$rgif > quantile(data.train$rgif, 0.999)] <- quantile(data.train$rgif, 0.999)
data.train$agif[data.train$agif > quantile(data.train$agif, 0.999)] <- quantile(data.train$agif, 0.999)

# check
par(mfrow = c(1,5))
boxplot(data.train$plow, main="plow")
abline(h = quantile(data.train$plow, 0.999), col="red")
abline(h = quantile(data.train$plow, 0.99), col="blue")
boxplot(data.train$tgif, main="tgif")
abline(h = quantile(data.train$tgif, 0.999), col="red")
abline(h = quantile(data.train$tgif, 0.99), col="blue")
boxplot(data.train$lgif, main="lgif")
abline(h = quantile(data.train$lgif, 0.999), col="red")
abline(h = quantile(data.train$lgif, 0.99), col="blue")
boxplot(data.train$rgif, main="rgif")
abline(h = quantile(data.train$rgif, 0.999), col="red")
abline(h = quantile(data.train$rgif, 0.99), col="blue")
boxplot(data.train$agif, main="agif")
abline(h = quantile(data.train$agif, 0.999), col="red")
abline(h = quantile(data.train$agif, 0.99), col="blue")

# trying out log transforms
par(mfrow = c(4,3))
hist(log(data.train$avhv)) # looks normally distributed
hist(log(data.train$incm)) # looks normally distributed
hist(log(data.train$inca)) # looks normally distributed
hist(log(data.train$plow + 1)) # not good
hist(log(data.train$npro)) # neg skewed; original was better so transform not needed
hist(log(data.train$tgif)) # looks normally distributed, slightly pos skewed
hist(log(data.train$lgif)) # looks normally distributed, slightly pos skewed
hist(log(data.train$rgif)) # looks normally distributed
hist(log(data.train$tdon)) # ok
hist(log(data.train$tlag)) # ok
hist(log(data.train$agif)) # looks normally distributed

par(mfrow = c(1,1))

# explore categorical variables
library(gmodels)
CrossTable(data.train$donr, data.train$reg1) # 20.5% of people live in REG1; 55.6% of them are donors
CrossTable(data.train$donr, data.train$reg2) # 33.6% of people live in REG2; 67.4% of them are donors
CrossTable(data.train$donr, data.train$reg3) # 12.3% of people live in REG3; 36.2% of them are donors
CrossTable(data.train$donr, data.train$reg4) # 13.5% of people live in REG4; 34.1% of them are donors
CrossTable(data.train$donr, data.train$home) # 88.3% of people are homeowners; 55.3% of them are donors
CrossTable(data.train$donr, data.train$chld) # 35.0% of people have no kids; 86.1% of them are donors
CrossTable(data.train$donr, data.train$hinc) # groups 3, 4*, 5 most likely donors
CrossTable(data.train$donr, data.train$genf) # females more likely to donate 60/40
CrossTable(data.train$donr, data.train$wrat) # higher wealth rating means more likely to donate

# look deeper into no children + homeowner group
data.train$nokid <- as.integer(ifelse(data.train$chld == 0, 1, 0))
CrossTable(data.train$home, data.train$nokid) # 92% of people with no children are homeowners
data.train$home_plus_nokid <- as.integer((data.train$home & data.train$nokid)*1)
CrossTable(data.train$donr, data.train$home_plus_nokid) # 32.2% of people are homeowners with no kids; 89.9% of them are donors

# look deeper into "middle class" household income group
data.train$midinc <- as.integer(ifelse(data.train$hinc == 3, 1, ifelse(data.train$hinc == 4, 1, ifelse(data.train$hinc == 5, 1, 0))))
CrossTable(data.train$donr, data.train$midinc) # 70.9% of people are middle class (wrt household income); 60.4% of them are donors

# process transformations and add new dummy variables to entire dataset before splitting out validation and test
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
charity.t$incm <- log(charity.t$incm)
charity.t$inca <- log(charity.t$inca)
charity.t$tgif <- log(charity.t$tgif)
charity.t$lgif <- log(charity.t$lgif)
charity.t$rgif <- log(charity.t$rgif)
charity.t$tdon <- log(charity.t$tdon)
charity.t$tlag <- log(charity.t$tlag)
charity.t$agif <- log(charity.t$agif)

charity.t$nokid <- as.integer(ifelse(charity.t$chld == 0, 1, 0))
charity.t$home_plus_nokid <- as.integer((charity.t$home & charity.t$nokid)*1)
charity.t$nokid <- NULL
charity.t$midinc <- as.integer(ifelse(charity.t$hinc == 3, 1, ifelse(charity.t$hinc == 4, 1, ifelse(charity.t$hinc == 5, 1, 0))))

##### DATA PREP ######################################################################################

# set up training set 
data.train <- charity.t[charity.t$part=="train",]
x.train <- data.train[,c(2:21,25:26)]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

# set up validation set
data.valid <- charity.t[charity.t$part=="valid",]
x.valid <- data.valid[,c(2:21,25:26)]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

# set up test set
data.test <- charity.t[charity.t$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,c(2:21,25:26)]

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)


##### CLASSIFICATION MODELING ########################################################################

# linear discriminant analysis
library(MASS)

#model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                  data.train.std.c) # include additional terms on the fly using I()
model.lda1 <- lda(donr ~ ., data.train.std.c)

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1, xlab="Mailings", ylab="Profit", main="Model: lda1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1355.0 11659.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
class.table <- table(chat.valid.lda1, c.valid) # classification table
class.table
#               c.valid
#chat.valid.lda1   0   1
#              0 655   8
#              1 364 991
# check n.mail.valid = 364+991 = 1355
# check profit = 14.5*991-2*1329 = 11659.5

model.name <- "lda1"
mailings.valid <- class.table[2,1] + class.table[2,2]
profit.valid <- (14.5*class.table[2,2]) - (2*mailings.valid)
df <- data.frame(cbind(model.name, mailings.valid, profit.valid))

######################################################################################################

# qda
model.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + home_plus_nokid +
                    midinc + avhv + I(avhv^2) + incm + I(incm^2) + inca + I(inca^2) + plow + I(plow^2) + 
                    npro + I(npro^2) + tgif + I(tgif^2) + lgif + I(lgif^2) + rgif + I(rgif^2) + tdon + 
                    I(tdon^2) + tlag + I(tlag^2) + agif + I(agif^2), data=data.train.std.c)

post.valid.qda1 <- predict(model.qda1, data.valid.std.c, type="response")$class # n.valid post probs

profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1, xlab="Mailings", ylab="Profit", main="Model: qda1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
# 1129.0 11009.5

#chat.valid.qda1 <- ifelse(post.valid.qda1 == 1, 1, 0) # mail to everyone above the cutoff
chat.valid.qda1 <- post.valid.qda1
class.table <- table(chat.valid.qda1, c.valid) # classification table
class.table
#                  c.valid
#chat.valid.qda1     0   1
#                0 805  84
#                1 214 915
# check n.mail.valid = 214 + 915 = 1129
# check profit = 14.5*915-2*1129 = 11009.5

# set up function to add results to table for model comparison
results.table = function(model.name){
  mailings.valid <- class.table[2,1] + class.table[2,2]
  profit.valid <- (14.5*class.table[2,2]) - (2*mailings.valid)
  df <<- rbind(df, data.frame(cbind(model.name, mailings.valid, profit.valid)))
  df
}
results.table(model.name="qda1")

######################################################################################################

# logistic regression

#model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                  data.train.std.c, family=binomial("logit"))
model.log1 <- glm(donr ~ ., data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1, xlab="Mailings", ylab="Profit", main="Model: log1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1307 11683

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
class.table <- table(chat.valid.log1, c.valid) # classification table
class.table
#               c.valid
#chat.valid.log1   0   1
#              0 698  13
#              1 321 986
# check n.mail.valid = 321+986 = 1307
# check profit = 14.5*986-2*1291 = 11683

results.table(model.name="log1")

######################################################################################################

# logistic regression GAM
library (gam)

#model.gamLR1 <- gam(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                      avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                    family=binomial, data=data.train.std.c)
model.gamLR1 <- gam(donr ~ ., family=binomial, data=data.train.std.c)

post.valid.gamLR1 <- predict(model.gamLR1, data.valid.std.c, type="response") # n.valid post probs

profit.gamLR1 <- cumsum(14.5*c.valid[order(post.valid.gamLR1, decreasing=T)]-2)
plot(profit.gamLR1, xlab="Mailings", ylab="Profit", main="Model: gamLR1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gamLR1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gamLR1)) # report number of mailings and maximum profit
# 1307 11683

cutoff.gamLR1 <- sort(post.valid.gamLR1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gamLR1 <- ifelse(post.valid.gamLR1 > cutoff.gamLR1, 1, 0) # mail to everyone above the cutoff
class.table <- table(chat.valid.gamLR1, c.valid) # classification table
class.table
#                  c.valid
#chat.valid.gamLR1   0   1
#                0 698  13
#                1 321 986
# check n.mail.valid = 321+986 = 1307
# check profit = 14.5*986-2*1307 = 11683

results.table(model.name="gamLR1")

######################################################################################################

# KNN
library(class)

set.seed(2)
model.knn1 <- knn(data.train.std.c, data.valid.std.c, c.train, k=3)

profit.knn1 <- cumsum(14.5*c.valid[order(model.knn1, decreasing=T)]-2)
plot(profit.knn1, xlab="Mailings", ylab="Profit", main="Model: knn1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.knn1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn1)) # report number of mailings and maximum profit
# 1107 11474

chat.valid.knn1 <- model.knn1
class.table <- table(chat.valid.knn1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 859  56
#              1 160 943
# check n.mail.valid = 160 + 943 = 1103
# check profit = 14.5*943-2*1100 = 11467.5

results.table(model.name="knn1")

######################################################################################################

# classification tree
library(tree)

model.tree1 <- tree(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data=data.train.std.c)
plot(model.tree1)
text(model.tree1, pretty = 0)

post.valid.tree1 <- predict(model.tree1, data.valid.std.c, type="class") # n.valid post probs

profit.tree1 <- cumsum(14.5*c.valid[order(post.valid.tree1, decreasing=T)]-2)
plot(profit.tree1, xlab="Mailings", ylab="Profit", main="Model: tree1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree1)) # report number of mailings and maximum profit
# 1139 11134.5

chat.valid.tree1 <- post.valid.tree1
class.table <- table(chat.valid.tree1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 807  75
#              1 212 924
# check n.mail.valid = 212 + 924 = 1136
# check profit = 14.5*924-2*1136 = 11126  #### DIFFERENT THAN ABOVE

results.table(model.name="tree1")


model.tree2 <- tree(as.factor(donr) ~ ., data=data.train.std.c)
plot(model.tree2)
text(model.tree2, pretty = 0)

post.valid.tree2 <- predict(model.tree2, data.valid.std.c, type="class") # n.valid post probs

profit.tree2 <- cumsum(14.5*c.valid[order(post.valid.tree2, decreasing=T)]-2)
plot(profit.tree2, xlab="Mailings", ylab="Profit", main="Model: tree2") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree2)) # report number of mailings and maximum profit
# 1133 10958

chat.valid.tree2 <- post.valid.tree2
class.table <- table(chat.valid.tree2, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 804  88
#              1 215 911
# check n.mail.valid = 215 + 911 = 1126 
# check profit = 14.5*911-2*1126 = 10957.5  #### DIFFERENT THAN ABOVE

results.table(model.name="tree2")

# pruning
set.seed(2)
cv.tree2 <- cv.tree(model.tree2, FUN = prune.misclass)
names(cv.tree2)
cv.tree2

par(mfrow = c(1,2))
plot(cv.tree2$size, cv.tree2$dev, type="b")
plot(cv.tree2$k, cv.tree2$dev, type="b")

prune.tree2 <- prune.misclass(model.tree2, best = 15)
par(mfrow = c(1,1))
plot(prune.tree2)
text(prune.tree2, pretty = 0)

post.valid.ptree2 <- predict(prune.tree2, data.valid.std.c, type="class") # n.valid post probs

profit.ptree2 <- cumsum(14.5*c.valid[order(post.valid.ptree2, decreasing=T)]-2)
plot(profit.ptree2, xlab="Mailings", ylab="Profit", main="Model: pruned tree2") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.ptree2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.ptree2)) # report number of mailings and maximum profit
# 1133 10958

chat.valid.ptree2 <- post.valid.ptree2
class.table <- table(chat.valid.ptree2, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 804  88
#              1 215 911
# check n.mail.valid = 215 + 911 = 1126
# check profit = 14.5*911-2*1126 = 10957.5

results.table(model.name="ptree2")

# pruning does not provide any benefit

######################################################################################################

# bagging
library(randomForest)

model.bag1 <- randomForest(as.factor(donr) ~ ., data=data.train.std.c, mtry=22)
model.bag1 # bagging with all 22 predictors at each split

post.valid.bag1 <- predict(model.bag1, data.valid.std.c, type="class") # n.valid post probs

profit.bag1 <- cumsum(14.5*c.valid[order(post.valid.bag1, decreasing=T)]-2)
plot(profit.bag1, xlab="Mailings", ylab="Profit", main="Model: bag1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.bag1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.bag1)) # report number of mailings and maximum profit
# 1055.0 11186.5

chat.valid.bag1 <- post.valid.bag1
class.table <- table(chat.valid.bag1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 881  82
#              1 138 917
# check n.mail.valid = 138 + 917 = 1055
# check profit = 14.5*917-2*1055 = 11186.5  

results.table(model.name="bag1")

######################################################################################################

# random forest

model.rf1 <- randomForest(as.factor(donr) ~ ., data=data.train.std.c)
model.rf1 # random forest using 4 predictors per split

post.valid.rf1 <- predict(model.rf1, data.valid.std.c, type="class") # n.valid post probs

profit.rf1 <- cumsum(14.5*c.valid[order(post.valid.rf1, decreasing=T)]-2)
plot(profit.rf1, xlab="Mailings", ylab="Profit", main="Model: rf1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf1)) # report number of mailings and maximum profit
# 1046.0 11146.5

chat.valid.rf1 <- post.valid.rf1
class.table <- table(chat.valid.rf1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 886  86
#              1 133 913
# check n.mail.valid = 133 + 913 = 1046
# check profit = 14.5*913-2*1046 = 11146.5

results.table(model.name="rf1")

######################################################################################################

# boosting
library(gbm)

set.seed(1)
model.boost1 <- gbm(donr ~ ., data=data.train.std.c, 
                    distribution = "bernoulli", n.trees = 5000, interaction.depth = 2)
model.boost1

post.valid.boost1 <- predict(model.boost1, newdata=data.valid.std.c, n.trees=5000, type="response") # n.valid post probs

profit.boost1 <- cumsum(14.5*c.valid[order(post.valid.boost1, decreasing=T)]-2)
plot(profit.boost1, xlab="Mailings", ylab="Profit", main="Model: boost1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boost1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boost1)) # report number of mailings and maximum profit
# 1331 11722

cutoff.boost1 <- sort(post.valid.boost1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.boost1 <- ifelse(post.valid.boost1 > cutoff.boost1, 1, 0) # mail to everyone above the cutoff
class.table <- table(chat.valid.boost1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 680   7
#              1 339 992
# check n.mail.valid = 339 + 992 = 1331
# check profit = 14.5*992-2*1331 = 11722

results.table(model.name="boost1")


set.seed(2)
model.boost2 <- gbm(donr ~ ., data=data.train.std.c, 
                    distribution = "adaboost", n.trees = 5000, interaction.depth = 2)
model.boost2

post.valid.boost2 <- predict(model.boost2, newdata=data.valid.std.c, n.trees=5000, type="response") # n.valid post probs

profit.boost2 <- cumsum(14.5*c.valid[order(post.valid.boost2, decreasing=T)]-2)
plot(profit.boost2, xlab="Mailings", ylab="Profit", main="Model: boost2") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boost2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boost2)) # report number of mailings and maximum profit
# 1310.0 11720.5

cutoff.boost2 <- sort(post.valid.boost2, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.boost2 <- ifelse(post.valid.boost2 > cutoff.boost2, 1, 0) # mail to everyone above the cutoff
class.table <- table(chat.valid.boost2, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 698  10
#              1 321 989
# check n.mail.valid = 321 + 989 = 1310
# check profit = 14.5*989-2*1261 = 11720.5

results.table(model.name="boost2")

######################################################################################################

# SVMs
library(e1071)

set.seed(2)
tune.out <- tune(svm, as.factor(donr) ~ ., data=data.train.std.c, kernel="linear",
                 ranges = list(cost=c(0.001, 0.005, 0.01, 0.05, 0.1, 1, 10))) 
summary(tune.out)

model.svc1 <- tune.out$best.model
summary(model.svc1)

post.valid.svc1 <- predict(model.svc1, newdata=data.valid.std.c) # n.valid post probs

profit.svc1 <- cumsum(14.5*c.valid[order(post.valid.svc1, decreasing=T)]-2)
plot(profit.svc1, xlab="Mailings", ylab="Profit", main="Model: svc1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svc1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svc1)) # report number of mailings and maximum profit
# 1054.0 10956.5

chat.valid.svc1 <- post.valid.svc1
class.table <- table(chat.valid.svc1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 866  98
#              1 153 901
# check n.mail.valid = 153 + 901 = 1054
# check profit = 14.5*901-2*1054 = 10956.5  #### DIFFERENT THAN ABOVE

results.table(model.name="svc1")


set.seed(2)
tune.out <- tune(svm, as.factor(donr) ~ ., data=data.train.std.c, kernel="radial",
                 ranges = list(cost = c(0.1, 1, 10, 100),
                               gamma = c(0.001, 0.005, 0.1))) # performed CV for various combos of cost/gamma to isolate best one
summary(tune.out)

model.svm1 <- tune.out$best.model
summary(model.svm1)

post.valid.svm1 <- predict(model.svm1, newdata=data.valid.std.c) # n.valid post probs

profit.svm1 <- cumsum(14.5*c.valid[order(post.valid.svm1, decreasing=T)]-2)
plot(profit.svm1, xlab="Mailings", ylab="Profit", main="Model: svm1") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm1)) # report number of mailings and maximum profit
# 1050 11124

chat.valid.svm1 <- post.valid.svm1
class.table <- table(chat.valid.svm1, c.valid) # classification table
class.table
#                c.valid
#chat.valid.knn1   0   1
#              0 881  87
#              1 138 912
# check n.mail.valid = 138 + 912 = 1050
# check profit = 14.5*912-2*1050 = 11124

results.table(model.name="svm1")

######################################################################################################

# Results

par(mfrow = c(4,3))
plot(profit.lda1, xlab="Mailings", ylab="Profit", main="Model: lda1")
plot(profit.qda1, xlab="Mailings", ylab="Profit", main="Model: qda1")
plot(profit.log1, xlab="Mailings", ylab="Profit", main="Model: log1")
plot(profit.knn1, xlab="Mailings", ylab="Profit", main="Model: knn1")
plot(profit.tree1, xlab="Mailings", ylab="Profit", main="Model: tree1")
plot(profit.tree2, xlab="Mailings", ylab="Profit", main="Model: tree2")
plot(profit.bag1, xlab="Mailings", ylab="Profit", main="Model: bag1")
plot(profit.rf1, xlab="Mailings", ylab="Profit", main="Model: rf1")
plot(profit.boost1, xlab="Mailings", ylab="Profit", main="Model: boost1")
plot(profit.boost2, xlab="Mailings", ylab="Profit", main="Model: boost2")
plot(profit.svc1, xlab="Mailings", ylab="Profit", main="Model: svc1")
plot(profit.svm1, xlab="Mailings", ylab="Profit", main="Model: svm1")

colnames(df) <- c("Model", "Mailings", "Profit")
df

#    Model Mailings  Profit
#1    lda1     1355 11659.5
#2    qda1     1129 11009.5
#3    log1     1307   11683
#4  gamLR1     1307   11683
#5    knn1     1103 11467.5
#6   tree1     1136   11126
#7   tree2     1126 10957.5
#8  ptree2     1126 10957.5
#9    bag1     1055 11186.5
#10    rf1     1046 11146.5
#11 boost1     1331   11722
#12 boost2     1310 11720.5
#13   svc1     1054 10956.5
#14   svm1     1050   11124

# select model.boost1 since it has maximum profit in the validation sample

post.test <- predict(model.boost1, newdata=data.test.std, n.trees=5000, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.boost1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1651  356
# based on this model we'll mail to the 356 highest posterior probabilities

# See below for saving chat.test into a file for submission



##### REGRESSION MODELING ############################################################################

# Least squares regression

#model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
#                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                data.train.std.y)
model.ls1 <- lm(damt ~ ., data.train.std.y)
summary(model.ls1) # Adjusted R-squared:  0.6362

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.543374
se <- sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1601235

model.name <- "ls1"
df2 <- data.frame(cbind(model.name, round(mpe,6), round(se,6)))
df2

# drop non-significant variables (manual selection)
model.ls2 <- lm(damt ~ . -npro - avhv - home_plus_nokid, data.train.std.y)
summary(model.ls2) # Adjusted R-squared:  0.6367 

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.542458
se <- sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1600718

model.name <- "ls2"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2



# choose variables using random forest
vars.rf <- randomForest(damt ~ ., data=data.valid.std.y, importance=T)
importance(vars.rf)
varImpPlot(vars.rf)

model.ls3 <- lm(damt ~ reg4 + lgif + agif + chld + hinc + home_plus_nokid + rgif + reg2 + reg3 + wrat + incm + plow, data.train.std.y)
summary(model.ls3) # Adjusted R-squared:  0.6218 

pred.valid.ls3 <- predict(model.ls3, newdata = data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.ls3)^2) # mean prediction error
# 1.605767
se <- sd((y.valid - pred.valid.ls3)^2)/sqrt(n.valid.y) # std error
# 0.159735

model.name <- "ls3"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# best subset selection with k-fold cross-validation
library(leaps)

# function to predict with regsubsets
predict.regsubsets <- function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

k <- 10
set.seed(2)
folds <- sample(1:k, nrow(data.train.std.y), replace=T)
cv.errors <- matrix(NA, k, 22, dimnames=list(NULL, paste(1:22)))

for(j in 1:k){
  best.fit <- regsubsets(damt ~ ., data=data.train.std.y[folds!=j,], nvmax=22)
  for(i in 1:22){
    pred = predict(best.fit, data.train.std.y[folds==j,], id=i)
    cv.errors[j,i] = mean((data.train.std.y$damt[folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
which.min(mean.cv.errors) # 15 variables

model.best1 <- regsubsets(damt ~ ., data=data.train.std.y, nvmax=22)
summary(model.best1)
coef(model.best1, 15)

pred.valid.best1 <- predict(model.best1, newdata = data.valid.std.y, id=15) # validation predictions
mpe <- mean((y.valid - pred.valid.best1)^2) # mean prediction error
# 1.54627
se <- sd((y.valid - pred.valid.best1)^2)/sqrt(n.valid.y) # std error
# 0.159534

model.name <- "best1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# ridge regression
library(glmnet)

x <- model.matrix(damt ~ ., data.train.std.y)[,-1]
grid <- 10^seq(10, -2, length=100)
model.ridge1 <- glmnet(x, data.train.std.y[,23], alpha=0, lambda=grid, standardize=F, thresh=1e-12)

plot(model.ridge1, xvar="lambda", label=T)

set.seed(2)
cv.out <- cv.glmnet(x, data.train.std.y[,23], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

xnew <- model.matrix(damt ~ ., data.valid.std.y)[,-1]
pred.valid.ridge1 <- predict(model.ridge1, newx = xnew, s=bestlam) # validation predictions
mpe <- mean((y.valid - pred.valid.ridge1)^2) # mean prediction error
# 1.570894
se <- sd((y.valid - pred.valid.ridge1)^2)/sqrt(n.valid.y) # std error
# 0.1625747

model.name <- "ridge1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# lasso

x <- model.matrix(damt ~ ., data.train.std.y)[,-1]
grid <- 10^seq(10, -2, length=100)
model.lasso1 <- glmnet(x, data.train.std.y[,23], alpha=1, lambda=grid, standardize=F)

plot(model.lasso1)

set.seed(2)
cv.out <- cv.glmnet(x, data.train.std.y[,23], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

xnew <- model.matrix(damt ~ ., data.valid.std.y)[,-1]
pred.valid.lasso1 <- predict(model.lasso1, newx = xnew, s=bestlam) # validation predictions
mpe <- mean((y.valid - pred.valid.lasso1)^2) # mean prediction error
# 1.553516
se <- sd((y.valid - pred.valid.lasso1)^2)/sqrt(n.valid.y) # std error
# 0.1603972

model.name <- "lasso1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# PCR
library(pls)

set.seed(2)
model.pcr1 <- pcr(damt ~ ., data=data.train.std.y, validation ="CV")
#scale=TRUE has the effect of standardizing each predictor
#validation="CV" causes pcr() to compute the ten-fold cross-validation error
# for each possible value of M

summary(model.pcr1)
validationplot(model.pcr1, val.type="MSEP")

# use 12 components
pred.valid.pcr1 <- predict(model.pcr1, newdata = data.valid.std.y, ncomp = 12) # validation predictions
mpe <- mean((y.valid - pred.valid.pcr1)^2) # mean prediction error
# 1.665712
se <- sd((y.valid - pred.valid.pcr1)^2)/sqrt(n.valid.y) # std error
# 0.161908

model.name <- "pcr1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

# try 18 components
pred.valid.pcr2 <- predict(model.pcr1, newdata = data.valid.std.y, ncomp = 18) # validation predictions
mpe <- mean((y.valid - pred.valid.pcr2)^2) # mean prediction error
# 1.583532
se <- sd((y.valid - pred.valid.pcr2)^2)/sqrt(n.valid.y) # std error
# 0.1623528

model.name <- "pcr2"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# PLS

set.seed(2)
model.pls1 <- plsr(damt ~ ., data=data.train.std.y, validation ="CV")
#validation="CV" causes pcr() to compute the ten-fold cross-validation error
# for each possible value of M

summary(model.pls1)
validationplot(model.pls1, val.type="MSEP")

# use 5 components
pred.valid.pls1 <- predict(model.pls1, newdata = data.valid.std.y, ncomp = 5) # validation predictions
mpe <- mean((y.valid - pred.valid.pls1)^2) # mean prediction error
# 1.665712
se <- sd((y.valid - pred.valid.pls1)^2)/sqrt(n.valid.y) # std error
# 0.161908

model.name <- "pls1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

# use 10 components
pred.valid.pls2 <- predict(model.pls1, newdata = data.valid.std.y, ncomp = 10) # validation predictions
mpe <- mean((y.valid - pred.valid.pls2)^2) # mean prediction error
# 1.665712
se <- sd((y.valid - pred.valid.pls2)^2)/sqrt(n.valid.y) # std error
# 0.161908

model.name <- "pls2"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# regression tree
library(tree)

model.rtree1 <- tree(damt ~ ., data=data.train.std.y)
plot(model.rtree1)
text(model.rtree1, pretty = 0)
summary(model.rtree1)

cv.rtree1 <- cv.tree(model.rtree1)
plot(cv.rtree1$size, cv.rtree1$dev, type = 'b')
# pruning would not help

pred.valid.rtree1 <- predict(model.rtree1, data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.rtree1)^2) # mean prediction error
# 2.270036
se <- sd((y.valid - pred.valid.rtree1)^2)/sqrt(n.valid.y) # std error
# 0.191232

model.name <- "rtree1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# bagging
library(randomForest)

model.rbag1 <- randomForest(damt ~ ., data=data.train.std.y, mtry=22)
model.rbag1 # bagging with all 22 predictors at each split

pred.valid.rbag1 <- predict(model.rbag1, data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.rbag1)^2) # mean prediction error
# 1.706642
se <- sd((y.valid - pred.valid.rbag1)^2)/sqrt(n.valid.y) # std error
# 0.175397

model.name <- "rbag1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2


######################################################################################################

# random forest

model.rrf1 <- randomForest(damt ~ ., data=data.train.std.y)
model.rrf1 # random forest using 7 predictors per split

pred.valid.rrf1 <- predict(model.rrf1, data.valid.std.y) # validation predictions
mpe <- mean((y.valid - pred.valid.rrf1)^2) # mean prediction error
# 1.661009
se <- sd((y.valid - pred.valid.rrf1)^2)/sqrt(n.valid.y) # std error
# 0.17358

model.name <- "rrf1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# boosting
library(gbm)

set.seed(2)
model.rboost1 <- gbm(damt ~ ., data=data.train.std.y, 
                    distribution = "gaussian", n.trees = 5000, interaction.depth = 2)
# A gradient boosted model with gaussian loss function.
model.rboost1 # 22 predictors of which 17 had non-zero influence.

pred.valid.rboost1 <- predict(model.rboost1, data.valid.std.y, n.trees = 5000) # validation predictions
mpe <- mean((y.valid - pred.valid.rboost1)^2) # mean prediction error
# 1.760334
se <- sd((y.valid - pred.valid.rboost1)^2)/sqrt(n.valid.y) # std error
# 0.173015

model.name <- "rboost1"
df2 <- rbind(df2, data.frame(cbind(model.name, round(mpe,6), round(se,6))))
df2

######################################################################################################

# Results

par(mfrow = c(5,3))
plot(x = y.valid, y = pred.valid.ls1, xlab="Actual values", ylab="Fitted values", main="Model: ls1")
lines(lowess(x = y.valid, y = pred.valid.ls1), col="red")
plot(x = y.valid, y = pred.valid.ls2, xlab="Actual values", ylab="Fitted values", main="Model: ls2")
lines(lowess(x = y.valid, y = pred.valid.ls2), col="red")
plot(x = y.valid, y = pred.valid.ls3, xlab="Actual values", ylab="Fitted values", main="Model: ls3")
lines(lowess(x = y.valid, y = pred.valid.ls3), col="red")
plot(x = y.valid, y = pred.valid.best1, xlab="Actual values", ylab="Fitted values", main="Model: best1")
lines(lowess(x = y.valid, y = pred.valid.best1), col="red")
plot(x = y.valid, y = pred.valid.pcr1, xlab="Actual values", ylab="Fitted values", main="Model: pcr1")
lines(lowess(x = y.valid, y = pred.valid.pcr1), col="red")
plot(x = y.valid, y = pred.valid.pcr2, xlab="Actual values", ylab="Fitted values", main="Model: pcr2")
lines(lowess(x = y.valid, y = pred.valid.pcr2), col="red")
plot(x = y.valid, y = pred.valid.pls1, xlab="Actual values", ylab="Fitted values", main="Model: pls1")
lines(lowess(x = y.valid, y = pred.valid.pls1), col="red")
plot(x = y.valid, y = pred.valid.pls2, xlab="Actual values", ylab="Fitted values", main="Model: pls2")
lines(lowess(x = y.valid, y = pred.valid.pls2), col="red")
plot(x = y.valid, y = pred.valid.ridge1, xlab="Actual values", ylab="Fitted values", main="Model: ridge1")
lines(lowess(x = y.valid, y = pred.valid.ridge1), col="red")
plot(x = y.valid, y = pred.valid.lasso1, xlab="Actual values", ylab="Fitted values", main="Model: lasso1")
lines(lowess(x = y.valid, y = pred.valid.lasso1), col="red")
plot(x = y.valid, y = pred.valid.rtree1, xlab="Actual values", ylab="Fitted values", main="Model: rtree1")
lines(lowess(x = y.valid, y = pred.valid.rtree1), col="red")
plot(x = y.valid, y = pred.valid.rbag1, xlab="Actual values", ylab="Fitted values", main="Model: rbag1")
lines(lowess(x = y.valid, y = pred.valid.rbag1), col="red")
plot(x = y.valid, y = pred.valid.rrf1, xlab="Actual values", ylab="Fitted values", main="Model: rrf1")
lines(lowess(x = y.valid, y = pred.valid.rrf1), col="red")
plot(x = y.valid, y = pred.valid.rboost1, xlab="Actual values", ylab="Fitted values", main="Model: rboost1")
lines(lowess(x = y.valid, y = pred.valid.rboost1), col="red")

colnames(df2) <- c("Model", "MPE", "Standard Error")
df2

#     Model      MPE Standard Error
#1      ls1 1.543374       0.160123
#2      ls2 1.542458       0.160072
#3      ls3 1.605767       0.159735
#4    best1  1.54627       0.159534
#5     pcr1 1.665712       0.161908
#6     pcr2 1.583532       0.162353
#7     pls1 1.563092       0.159341
#8     pls2 1.543214        0.16007
#9   ridge1 1.570894       0.162575
#10  lasso1 1.553516       0.160397
#11  rtree1 2.270036       0.191232
#12   rbag1 2.270036       0.191232
#13   rbag1 1.706642       0.175397
#14    rrf1 1.661009        0.17358
#15 rboost1 1.760334       0.173015

# select model.ls2 since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions


######################################################################################################

# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt
sum(chat.test) # 356 mailings

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="CJA.csv", row.names=FALSE)

# submit the csv file in Canvas for evaluation based on actual test donr and damt values
