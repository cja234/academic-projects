# PREDICT 456 Sports Performance Analysis Section 55 Summer 2016
# Christopher Anderson
# Assignment #4
# Last modified August 28, 2016

library(moments)
library(ggplot2)
library(gridExtra)
library(pitchRx)
library(Hmisc)
library(psych)
library(lattice)
library(FSA)
library(asbio)
library(dplyr)

# Read in PITCHf/x data downloaded from MLB Statcast Search tool
mydata <- read.csv("savant_data.csv", header = T, sep = ",")

# Subset the data to include only relevant columns (541581 obs. of 12 variables)
mydata <- subset(mydata, select=c("pitch_type","description","events","type","start_speed","effective_speed",
                                  "spin_rate","break_angle","break_length","hit_speed","stand","p_throws"))

# Remove records with missing data
mydata <- na.omit(mydata) # 528486 obs. of  12 variables; 13095 obs. removed

# Examine structure of data
str(mydata)

# Amend data types
mydata$start_speed <- as.numeric(as.character(mydata$start_speed))
mydata$effective_speed <- as.numeric(as.character(mydata$effective_speed))
mydata$spin_rate <- as.numeric(as.character(mydata$spin_rate))
mydata$break_angle <- as.numeric(as.character(mydata$break_angle))
mydata$break_length <- as.numeric(as.character(mydata$break_length))
mydata$hit_speed <- as.numeric(as.character(mydata$hit_speed))
str(mydata)
head(mydata)  
tail(mydata)

# Summary statistics by pitch type
aggregate(cbind(start_speed, effective_speed, spin_rate, break_angle, break_length, hit_speed) ~ pitch_type, data = mydata, mean)
summary(mydata$pitch_type)

# Exploratory boxplots of pitch measurements by pitch type
plot1 <- ggplot(data = mydata, aes(x = pitch_type, y = start_speed, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
plot2 <- ggplot(data = mydata, aes(x = pitch_type, y = effective_speed, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
plot3 <- ggplot(data = mydata, aes(x = pitch_type, y = spin_rate, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
plot4 <- ggplot(data = mydata, aes(x = pitch_type, y = break_angle, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
plot5 <- ggplot(data = mydata, aes(x = pitch_type, y = break_length, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
plot6 <- ggplot(data = mydata, aes(x = pitch_type, y = hit_speed, group = pitch_type, 
                                   colour = pitch_type))+ geom_boxplot(show.legend = FALSE)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

# Exploratory histograms and normality assessment
par(mfrow = c(3,2), oma=c(0,0,2,0))
hist(mydata$start_speed, xlab = "Start Speed", main = "", col = "steelblue")
hist(mydata$effective_speed, xlab = "Effective Speed", main = "", col = "steelblue")
hist(mydata$spin_rate, xlab = "Spin Rate", main = "", col = "steelblue")
hist(mydata$break_angle, xlab = "Break Angle", main = "", col = "steelblue")
hist(mydata$break_length, xlab = "Break Length", main = "", col = "steelblue")
hist(mydata$hit_speed, xlab = "Hit Speed", main = "", col = "steelblue")
title("", outer=TRUE)
par(mfrow = c(1,1))

# Exploratory histograms within each pitch type
plot1 <- histogram(~ start_speed | pitch_type, data=mydata)
plot2 <- histogram(~ effective_speed | pitch_type, data=mydata)
plot3 <- histogram(~ spin_rate | pitch_type, data=mydata)
plot4 <- histogram(~ break_angle | pitch_type, data=mydata)
plot5 <- histogram(~ break_length | pitch_type, data=mydata)
plot6 <- histogram(~ hit_speed | pitch_type, data=mydata)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

# Kruskal-Wallis test for hit speed by pitch type
qchisq(0.950, 14) # Critical chi-squared value = 23.68479
kruskal.test(hit_speed ~ pitch_type, data = mydata) # Kruskal-Wallis chi-squared = 1118.9, df = 14, p-value < 2.2e-16
pairw.kw(y = mydata$hit_speed, x = mydata$pitch_type, conf = .95)

# Batting average on balls in play
# Filter to include only one record per at bat
BABIPdata <- 
  filter(mydata, description == "In play, no out"| description == "In play, out(s)"| description == "In play, run(s)") 
  # 95723 obs. of 12 variables
BABIPdata <- filter(BABIPdata, hit_speed > 0) # 84621 obs. of 12 variables; 11102 obs. removed

# Identify each observation as a Hit or not
H <- c("Single", "Double", "Triple", "Home Run")

# Identify each observationl as an official at bat or not (events like walks and hit by pitches are excluded)
AB <- c("Single", "Double", "Triple", "Home Run","Bunt Groundout","Bunt Lineout","Bunt Pop Out","Double Play","Field Error",
        "Fielders Choice","Fielders Choice Out","Flyout","Forceout","Grounded Into DP","Groundout","Lineout","Pop Out",
        "Strikeout","Strikeout - DP","Triple Play")

# Calculate BABIP per pitch type and put results in dataframe 
Hit <- factor(BABIPdata$events %in% H, labels = c(0, 1))
AtBats <- factor(BABIPdata$events %in% AB, labels = c(0, 1))
BABIPdata <- data.frame(BABIPdata, Hit, AtBats) # 84621 obs. of 14 variables 

BABIPdata$Hit <- as.numeric(as.character(BABIPdata$Hit))
BABIPdata$AtBats <- as.numeric(as.character(BABIPdata$AtBats))

Hit.type <- aggregate(Hit ~ pitch_type, data = BABIPdata, sum)
AtBats.type <- aggregate(AtBats ~ pitch_type, data = BABIPdata, sum)
BABIP <- round(Hit.type$Hit / AtBats.type$AtBats, 3)
BABIP <- data.frame(Hit.type$pitch_type, BABIP)
colnames(BABIP) <- c("pitch_type","BABIP")
BABIPdata <- merge(BABIPdata, BABIP, by = 'pitch_type')

# View BABIP by pitch type
BABIP[order(BABIP$BABIP, decreasing = FALSE),]

# Kruskal-Wallis test for BABIP by pitch type
kruskal.test(BABIP ~ pitch_type, data = BABIPdata)
pairw.kw(y = BABIPdata$BABIP, x = BABIPdata$pitch_type, conf = .95)


# Save datasets for future use
write.csv(mydata, file = "assignment4_mydata.csv")
write.csv(BABIPdata, file = "assignment4_BABIPdata.csv")

# End