#PREDICT 456 Sports Performance Analysis Section 55 Summer 2016
#Christopher Anderson
#Assignment #1

library(moments)
library(ggplot2)
library(gridExtra)
library(pitchRx)
library(Hmisc)

pitchdata <- read.csv("savant_data.csv", header = T, sep = ",")
hitdata <- read.csv("savant_data2.csv", header = T, sep = ",")

# Subset the data to include only relevant columns (348728 obs. of 11 variables)
pitchdata <- subset(pitchdata, select=c("height","p_throws","pitch_type","z0","pz","pfx_z","release_extension",
                                        "start_speed","effective_speed","hit_speed","hit_angle"))
hitdata <- subset(hitdata, select=c("height","p_throws","pitch_type","z0","pz","pfx_z","release_extension",
                                    "start_speed","effective_speed","hit_speed","hit_angle"))

# Remove records with missing data
pitchdata <- na.omit(pitchdata) # 335633 obs. of  11 variables (all pitches thrown); 13095 removed
hitdata <- na.omit(hitdata) # 60515 obs. of  11 variables (batted balls); 288213 removed

# Examine structure of data and summary statistics
str(pitchdata)
head(pitchdata)  
tail(pitchdata)  
grid.table(summary(pitchdata))
dev.off()
str(hitdata)
head(hitdata)  
tail(hitdata)  
grid.table(summary(hitdata))
dev.off()

# Exploratory histograms and normality assessment for all pitches dataset
hist(pitchdata$height, main = "Figure 1: Height, total pitches thrown", col = "steelblue")
skewness(pitchdata$height)
kurtosis(pitchdata$height)
par(mfrow = c(3,2), oma=c(0,0,2,0))
hist(pitchdata$z0, xlab = "Release Height", main = "", col = "steelblue")
hist(pitchdata$pz, xlab = "Height at Home Plate", main = "", col = "steelblue")
hist(pitchdata$pfx_z, xlab = "Vertical Movement", main = "", col = "steelblue")
hist(pitchdata$release_extension, xlab = "Release Extension", main = "", col = "steelblue")
hist(pitchdata$start_speed, xlab = "Velocity", main = "", col = "steelblue")
hist(pitchdata$effective_speed, xlab = "Perceived Velocity", main = "", col = "steelblue")
title("Figure 2: Histograms, all pitches thrown", outer=TRUE)
par(mfrow = c(1,1))
skewness(pitchdata$z0)
kurtosis(pitchdata$z0)
skewness(pitchdata$pz)
kurtosis(pitchdata$pz)
skewness(pitchdata$pfx_z)
kurtosis(pitchdata$pfx_z)
skewness(pitchdata$release_extension)
kurtosis(pitchdata$release_extension)
skewness(pitchdata$start_speed)
kurtosis(pitchdata$start_speed)
skewness(pitchdata$effective_speed)
kurtosis(pitchdata$effective_speed)

# Exploratory histograms and normality assessment for batted ball data
hist(hitdata$height, main = "Height, batted balls only", col = "steelblue")
skewness(hitdata$height)
kurtosis(hitdata$height)
par(mfrow = c(3,2), oma=c(0,0,2,0))
hist(hitdata$z0, xlab = "Release Height", main = "", col = "steelblue")
hist(hitdata$pz, xlab = "Height at Home Plate", main = "", col = "steelblue")
hist(hitdata$pfx_z, xlab = "Vertical Movement", main = "", col = "steelblue")
hist(hitdata$release_extension, xlab = "Release Extension", main = "", col = "steelblue")
hist(hitdata$start_speed, xlab = "Velocity", main = "", col = "steelblue")
hist(hitdata$effective_speed, xlab = "Perceived Velocity", main = "", col = "steelblue")
title("Figure 3: Histograms, batted balls only", outer=TRUE)
par(mfrow = c(1,1))
skewness(hitdata$z0)
kurtosis(hitdata$z0)
skewness(hitdata$pz)
kurtosis(hitdata$pz)
skewness(hitdata$pfx_z)
kurtosis(hitdata$pfx_z)
skewness(hitdata$release_extension)
kurtosis(hitdata$release_extension)
skewness(hitdata$start_speed)
kurtosis(hitdata$start_speed)
skewness(hitdata$effective_speed)
kurtosis(hitdata$effective_speed)

# Check outliers using boxplots
boxplot(pitchdata$height, main = "Figure 4: Height, all pitches thrown", horizontal = TRUE)
par(mfrow = c(3,2), oma=c(0,0,2,0))
boxplot(pitchdata$z0, xlab = "Release Height", main = "", horizontal = TRUE)
boxplot(pitchdata$pz, xlab = "Height at Home Plate", main = "", horizontal = TRUE)
boxplot(pitchdata$pfx_z, xlab = "Vertical Movement", main = "", horizontal = TRUE)
boxplot(pitchdata$release_extension, xlab = "Release Extension", main = "", horizontal = TRUE)
boxplot(pitchdata$start_speed, xlab = "Velocity", main = "", horizontal = TRUE)
boxplot(pitchdata$effective_speed, xlab = "Perceived Velocity", main = "", horizontal = TRUE)
title("Figure 5: Boxplots, all pitches thrown", outer=TRUE)
par(mfrow = c(1,1))

par(mfrow = c(3,2), oma=c(0,0,2,0))
boxplot(hitdata$z0, xlab = "Release Height", main = "", horizontal = TRUE)
boxplot(hitdata$pz, xlab = "Height at Home Plate", main = "", horizontal = TRUE)
boxplot(hitdata$pfx_z, xlab = "Vertical Movement", main = "", horizontal = TRUE)
boxplot(hitdata$release_extension, xlab = "Release Extension", main = "", horizontal = TRUE)
boxplot(hitdata$start_speed, xlab = "Velocity", main = "", horizontal = TRUE)
boxplot(hitdata$effective_speed, xlab = "Perceived Velocity", main = "", horizontal = TRUE)
title("Figure 6: Boxplots, batted balls only", outer=TRUE)
par(mfrow = c(1,1))

# Scatterplots to examine relationships between variables for all pitches thrown data
plot1 <- ggplot(data=pitchdata, aes(x=height, y=z0)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Release Height")
plot2 <- ggplot(data=pitchdata, aes(x=height, y=pz)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Height at Home Plate")
plot3 <- ggplot(data=pitchdata, aes(x=height, y=pfx_z)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Vertical Movement")
plot4 <- ggplot(data=pitchdata, aes(x=height, y=release_extension)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Release Extension")
plot5 <- ggplot(data=pitchdata, aes(x=height, y=start_speed)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Velocity")
plot6 <- ggplot(data=pitchdata, aes(x=height, y=effective_speed)) + geom_point(size=2) +
                  ggtitle("Plot of Pitcher Height vs Perceived Velocity")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

# Correlations with all pitches thrown data
cor.test(pitchdata$height, pitchdata$z0)
cor.test(pitchdata$height, pitchdata$z0, method="spearman")
cor.test(pitchdata$height, pitchdata$pz)
cor.test(pitchdata$height, pitchdata$pz, method="spearman")
cor.test(pitchdata$height, pitchdata$pfx_z)
cor.test(pitchdata$height, pitchdata$pfx_z, method="spearman")
cor.test(pitchdata$height, pitchdata$release_extension)
cor.test(pitchdata$height, pitchdata$release_extension, method="spearman")
cor.test(pitchdata$height, pitchdata$start_speed)
cor.test(pitchdata$height, pitchdata$start_speed, method="spearman")
cor.test(pitchdata$height, pitchdata$effective_speed)
cor.test(pitchdata$height, pitchdata$effective_speed, method="spearman")

# Create new variable for "downward plane"
# Height at release minus height as the ball crosses home plate
downplane <- pitchdata$z0-pitchdata$pz
pitchdata <- data.frame(pitchdata[1:6], downplane, pitchdata[7:11])
downplane_hit <- hitdata$z0-hitdata$pz
hitdata <- data.frame(hitdata[1:6], downplane_hit, hitdata[7:11])

cor.test(pitchdata$height, pitchdata$downplane)
cor.test(pitchdata$height, pitchdata$downplane, method="spearman")

# Correlations with exit velocity 
# Batted balls only data
cor.test(y = hitdata$hit_speed, x = hitdata$height)
cor.test(y = hitdata$hit_speed, x = hitdata$height, method="spearman")
cor.test(y = hitdata$hit_speed, x = hitdata$z0)
cor.test(y = hitdata$hit_speed, x = hitdata$z0, method="spearman")
cor.test(y = hitdata$hit_speed, x = hitdata$downplane_hit)
cor.test(y = hitdata$hit_speed, x = hitdata$downplane_hit, method="spearman")
cor.test(y = hitdata$hit_speed, x = hitdata$release_extension)
cor.test(y = hitdata$hit_speed, x = hitdata$release_extension, method="spearman")
cor.test(y = hitdata$hit_speed, x = hitdata$effective_speed)
cor.test(y = hitdata$hit_speed, x = hitdata$effective_speed, method="spearman")

# Correlations with launch angle 
# Batted balls only data
cor.test(y = hitdata$hit_angle, x = hitdata$height)
cor.test(y = hitdata$hit_angle, x = hitdata$height, method="spearman")
cor.test(y = hitdata$hit_angle, x = hitdata$z0)
cor.test(y = hitdata$hit_angle, x = hitdata$z0, method="spearman")
cor.test(y = hitdata$hit_angle, x = hitdata$downplane_hit)
cor.test(y = hitdata$hit_angle, x = hitdata$downplane_hit, method="spearman")
cor.test(y = hitdata$hit_angle, x = hitdata$release_extension)
cor.test(y = hitdata$hit_angle, x = hitdata$release_extension, method="spearman")
cor.test(y = hitdata$hit_angle, x = hitdata$effective_speed)
cor.test(y = hitdata$hit_angle, x = hitdata$effective_speed, method="spearman")

# Create new variable for "result"
# Classification of result as Ground ball, line drive, fly ball, or pop up
result <- cut(hitdata$hit_angle, c(-180,10,25,50,180), c("Ground Ball","Line Drive","Fly Ball","Pop Up"))
hitdata <- data.frame(hitdata, result)
# Create new variable "tallshort"
# Classification of pitcher as tall or short based on mean height
tallshort <- cut(hitdata$height, c(60,74.67,90), c("Short","Tall"))
hitdata <- data.frame(hitdata[1], tallshort, hitdata[2:13])

# Scatterplots of interesting relationships between variables for all batted balls data
plot1 <- ggplot(data=hitdata, aes(x=z0, y=hit_speed, colour=result)) + geom_point(size=2) +
  ggtitle("Plot of Release Height vs Exit Velocity, colored by Result")
plot2 <- ggplot(data=hitdata, aes(x=release_extension, y=hit_speed, colour=result)) + geom_point(size=2) +
  ggtitle("Plot of Release Extension vs Exit Velocity, colored by Result")
plot3 <- ggplot(data=hitdata, aes(x=downplane_hit, y=hit_speed, colour=result)) + geom_point(size=2.5) +
  ggtitle("Plot of Downward Plane vs Exit Velocity, colored by Result")
plot4 <- ggplot(data=hitdata, aes(x=downplane_hit, y=hit_speed, colour=tallshort)) + geom_point(size=2.5) +
  ggtitle("Plot of Downward Plane vs Exit Velocity, colored by Tall/Short")
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

plot1 <- ggplot(data=hitdata, aes(x=downplane_hit, y=hit_angle, colour=result)) + geom_point(size=4) +
  ggtitle("Plot of Downward Plane vs Launch Angle")
plot2 <- ggplot(data=hitdata, aes(x=downplane_hit, y=hit_angle, colour=tallshort)) + geom_point(size=4) +
  ggtitle("Plot of Downward Plane vs Launch Angle")
grid.arrange(plot1, plot2, ncol=2)

# Save datasets for future use
write.csv(pitchdata, file = "pitchdata.csv")
write.csv(hitdata, file = "hitdata.csv")