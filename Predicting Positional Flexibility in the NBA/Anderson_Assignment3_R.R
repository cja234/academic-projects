#PREDICT 456 Sports Performance Analysis Section 55 Summer 2016
#Christopher Anderson
#Assignment #3

library(moments)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(dplyr)
library("psych")

# Read in NBA Combine data downloaded from DraftExpress 
combine <- read.csv("NBADraftCombine.csv", header = T, sep = ",")
# 458 observations of 19 variables 

# Read in player positions and statistics data downloaded from Basketball-Reference
stats <- read.csv("play-index_psl_finder.cgi_stats.csv", header = T, sep = ",")
# 361 observations of 34 variables 

# Merge the datasets together
mydata <- merge(x = combine, y = stats, by.x = "Name", by.y = "Player")
mydata <- subset(mydata, select=c("Name","Height.w.o.Shoes","Weight","Body.Fat","Wingspan","Max.Vert",
                                  "Bench","Agility","Sprint","TRB","AST","STL","BLK","TOV","PTS","POS"))
colnames(mydata) <- c("Name","Height","Weight","Body_Fat","Wingspan","Vertical","Bench","Agility","Sprint",
                      "TRB","AST","STL","BLK","TOV","PTS","Position")
# 215 observations of 16 variables 

# Examine structure of data and summary statistics
str(mydata)
head(mydata)  
tail(mydata)  
summary(mydata)

# Exploratory scatter plot matrix with correlations and histograms
pairs.panels(mydata[2:15], cex = 1.25, cex.labels = 1.75)

# Exploratory boxplots of combine data by position
plot1 <- ggplot(data = mydata, aes(x = Position, y = Height, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot2 <- ggplot(data = mydata, aes(x = Position, y = Weight, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot3 <- ggplot(data = mydata, aes(x = Position, y = Body_Fat, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot4 <- ggplot(data = mydata, aes(x = Position, y = Wingspan, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot5 <- ggplot(data = mydata, aes(x = Position, y = Vertical, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot6 <- ggplot(data = mydata, aes(x = Position, y = Bench, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot7 <- ggplot(data = mydata, aes(x = Position, y = Agility, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot8 <- ggplot(data = mydata, aes(x = Position, y = Sprint, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2)

# Exploratory boxplots of important statistics by position
plot1 <- ggplot(data = mydata, aes(x = Position, y = TRB, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot2 <- ggplot(data = mydata, aes(x = Position, y = AST, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot3 <- ggplot(data = mydata, aes(x = Position, y = STL, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot4 <- ggplot(data = mydata, aes(x = Position, y = BLK, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot5 <- ggplot(data = mydata, aes(x = Position, y = TOV, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
plot6 <- ggplot(data = mydata, aes(x = Position, y = PTS, group = Position, 
                                   colour = Position))+ geom_boxplot(show.legend = FALSE)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)


# Filter out tallest and shortest quantiles of players to focus on "small ball" players
# For later use
q25 <- quantile(mydata$Height, probs = 0.25, na.rm = TRUE)
q75 <- quantile(mydata$Height, probs = 0.75, na.rm = TRUE)
q25 <- as.numeric(as.character(q25))
q75 <- as.numeric(as.character(q75))
flex25 <- factor(mydata$Height < q25, labels = c("yes", "no"))
flex75 <- factor(mydata$Height > q75, labels = c("yes", "no"))
flexdata <- data.frame(mydata, flex25, flex75)
flexdata <- filter(flexdata, flex25 == "yes")
flexdata <- filter(flexdata, flex75 == "yes")

# Create binary variable for each position
PG <- factor(mydata$Position == "PG", labels = c(0,1))
SG <- factor(mydata$Position == "SG", labels = c(0,1))
SF <- factor(mydata$Position == "SF", labels = c(0,1))
PF <- factor(mydata$Position == "PF", labels = c(0,1))
C <- factor(mydata$Position == "C", labels = c(0,1))
mydata <- data.frame(mydata, PG, SG, SF, PF, C)

# Create binary variable for each position for flexdata (for later use)
flexPG <- factor(flexdata$Position == "PG", labels = c(0,1))
flexSG <- factor(flexdata$Position == "SG", labels = c(0,1))
flexSF <- factor(flexdata$Position == "SF", labels = c(0,1))
flexPF <- factor(flexdata$Position == "PF", labels = c(0,1))
flexC <- factor(flexdata$Position == "C", labels = c(0,1))
flexdata <- data.frame(flexdata, flexPG, flexSG, flexSF, flexPF, flexC)

# Logistic regressions for each position using all NBA Combine measurements except height and weight
PGlog <- glm(PG ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=mydata, family = binomial("logit"))
PG_Prob <- predict(PGlog, mydata, type = "response")
SGlog <- glm(SG ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=mydata, family = binomial("logit"))
SG_Prob <- predict(SGlog, mydata, type = "response")
SFlog <- glm(SF ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=mydata, family = binomial("logit"))
SF_Prob <- predict(SFlog, mydata, type = "response")
PFlog <- glm(PF ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=mydata, family = binomial("logit"))
PF_Prob <- predict(PFlog, mydata, type = "response")
Clog <- glm(C ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
            data=mydata, family = binomial("logit"))
C_Prob <- predict(Clog, mydata, type = "response")
PGlog  # Residual Deviance: 79.46 	AIC: 93.46
SGlog  # Residual Deviance: 175.7 	AIC: 189.7
SFlog  # Residual Deviance: 161.8 	AIC: 175.8
PFlog  # Residual Deviance: 137.6 	AIC: 151.6
Clog   # Residual Deviance: 83.11 	AIC: 97.11
results <- data.frame(mydata[c(1,16)], PG_Prob, SG_Prob, SF_Prob, PF_Prob, C_Prob)
results$PG_Prob <- round(as.numeric(as.character(results$PG_Prob)),2)
results$SG_Prob <- round(as.numeric(as.character(results$SG_Prob)),2)
results$SF_Prob <- round(as.numeric(as.character(results$SF_Prob)),2)
results$PF_Prob <- round(as.numeric(as.character(results$PF_Prob)),2)
results$C_Prob <- round(as.numeric(as.character(results$C_Prob)),2)
Flex_Score <- results$PG_Prob + results$SG_Prob + results$SF_Prob + results$PF_Prob + results$C_Prob
results <- data.frame(results, Flex_Score)
View(results)

# Logistic regressions for each position using all NBA Combine measurements except height and weight using flexdata
PGlog <- glm(flexPG ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=flexdata, family = binomial("logit"))
PG_Prob <- predict(PGlog, flexdata, type = "response")
SGlog <- glm(flexSG ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=flexdata, family = binomial("logit"))
SG_Prob <- predict(SGlog, flexdata, type = "response")
SFlog <- glm(flexSF ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=flexdata, family = binomial("logit"))
SF_Prob <- predict(SFlog, flexdata, type = "response")
PFlog <- glm(flexPF ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
             data=flexdata, family = binomial("logit"))
PF_Prob <- predict(PFlog, flexdata, type = "response")
Clog <- glm(flexC ~ Body_Fat + Wingspan + Vertical + Bench + Agility + Sprint, 
            data=flexdata, family = binomial("logit"))
C_Prob <- predict(Clog, flexdata, type = "response")
PGlog  # Residual Deviance: 1.263e-07 	AIC: 14
SGlog  # Residual Deviance: 80.15 	AIC: 94.15
SFlog  # Residual Deviance: 107.3 	AIC: 121.3
PFlog  # Residual Deviance: 55.63 	AIC: 69.63
Clog   # Residual Deviance: 2.984e-09 	AIC: 14
flexresults <- data.frame(flexdata[c(1,16)], PG_Prob, SG_Prob, SF_Prob, PF_Prob, C_Prob)
flexresults$PG_Prob <- round(as.numeric(as.character(flexresults$PG_Prob)),2)
flexresults$SG_Prob <- round(as.numeric(as.character(flexresults$SG_Prob)),2)
flexresults$SF_Prob <- round(as.numeric(as.character(flexresults$SF_Prob)),2)
flexresults$PF_Prob <- round(as.numeric(as.character(flexresults$PF_Prob)),2)
flexresults$C_Prob <- round(as.numeric(as.character(flexresults$C_Prob)),2)
Flex_Score <- flexresults$PG_Prob + flexresults$SG_Prob + flexresults$SF_Prob + flexresults$PF_Prob + flexresults$C_Prob
flexresults <- data.frame(flexresults, Flex_Score)
View(flexresults)


# Save datasets for future use
write.csv(mydata, file = "assignment3_mydata.csv")
write.csv(flexdata, file = "assignment3_flexdata.csv")
write.csv(results, file = "assignment3_results.csv")
write.csv(flexresults, file = "assignment3_flexresults.csv")

# End
