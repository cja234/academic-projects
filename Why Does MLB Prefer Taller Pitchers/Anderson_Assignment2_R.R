#PREDICT 456 Sports Performance Analysis Section 55 Summer 2016
#Christopher Anderson
#Assignment #2

library(moments)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(dplyr)
library(XML)

# Read in pitching results data downloaded from MLB Statcast
results <- read.csv("results.csv", header = T, sep = ",")

# Download and merge player height data from Baseball Prospectus active player tables
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2008&this_lvl=MLB")
height2008 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2009&this_lvl=MLB")
height2009 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2010&this_lvl=MLB")
height2010 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2011&this_lvl=MLB")
height2011 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2012&this_lvl=MLB")
height2012 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2013&this_lvl=MLB")
height2013 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2014&this_lvl=MLB")
height2014 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2015&this_lvl=MLB")
height2015 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)
d <- readHTMLTable("http://www.baseballprospectus.com/sortable/extras/active_players.php?this_year=2016&this_lvl=MLB")
height2016 <- data.frame(d$pitchers_list_datagrid$`MLB ID`, d$pitchers_list_datagrid$Height, stringsAsFactors = FALSE)

height <- rbind(height2008, height2009, height2010, height2011, height2012, height2013, height2014, height2015, height2016)
rm(list = c("d","height2008","height2009","height2010","height2011","height2012","height2013","height2014","height2015","height2016"))
height <- unique(height)
colnames(height) <- c("player_id", "height")

mydata <- merge(x = results, y = height, by = "player_id")
mydata <- subset(mydata, select=c("player_id","player_name","height","pitches","abs","velocity","effective_speed","spin_rate",
                                  "exit_velocity","launch_angle","ba","babip","slg","iso"))
mydata$height <- as.numeric(as.character(mydata$height))
mydata$player_name <- as.character(mydata$player_name)

# Dichotimize the data into tall and short pitchers
tall_short <- factor(mydata$height > mean(mydata$height), labels = c("short", "tall"))
mydata <- data.frame(mydata, tall_short)
talldata <- filter(mydata, tall_short == "tall")
shortdata <- filter(mydata, tall_short == "short")
# 1653 total observations 814 tall 839 short

# Examine structure of data and summary statistics
str(mydata)
head(mydata)  
tail(mydata)  
grid.table(summary(mydata[,3:15]))
dev.off()

# Exploratory histograms and normality assessment
par(mfrow = c(1,3), oma=c(0,0,2,0))
hist(shortdata$velocity, main = "", col = "light blue")
hist(talldata$velocity, main = "", col = "royal blue")
boxplot(velocity ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Velocity by Height", outer=TRUE)
skewness(shortdata$velocity)
kurtosis(shortdata$velocity)
skewness(talldata$velocity)
kurtosis(talldata$velocity)

# Filter out extreme outliers based on known knuckleball pitchers
mydata <- filter(mydata, player_name != "Tim Wakefield")
mydata <- filter(mydata, player_name != "Charlie Zink")
mydata <- filter(mydata, player_name != "Charlie Haeger")
mydata <- filter(mydata, player_name != "Steven Wright")
mydata <- filter(mydata, player_name != "R.A. Dickey")
talldata <- filter(mydata, tall_short == "tall")
shortdata <- filter(mydata, tall_short == "short")
# 5 observations removed; 1648 remaining 813 tall 835 short

# Continue exploratory histograms and normality assessment
par(mfrow = c(1,3), oma=c(0,0,2,0))
hist(shortdata$velocity, main = "", col = "light blue")
hist(talldata$velocity, main = "", col = "royal blue")
boxplot(velocity ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Velocity by Height", outer=TRUE)
skewness(shortdata$velocity)
kurtosis(shortdata$velocity)
skewness(talldata$velocity)
kurtosis(talldata$velocity)

hist(shortdata$effective_speed, main = "", col = "light blue")
hist(talldata$effective_speed, main = "", col = "royal blue")
boxplot(effective_speed ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Effective_speed by Height", outer=TRUE)
skewness(shortdata$effective_speed, na.rm = TRUE)
kurtosis(shortdata$effective_speed, na.rm = TRUE)
skewness(talldata$effective_speed, na.rm = TRUE)
kurtosis(talldata$effective_speed, na.rm = TRUE)

hist(shortdata$spin_rate, main = "", col = "light blue")
hist(talldata$spin_rate, main = "", col = "royal blue")
boxplot(spin_rate ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Spin_rate by Height", outer=TRUE)
skewness(shortdata$spin_rate, na.rm = TRUE)
kurtosis(shortdata$spin_rate, na.rm = TRUE)
skewness(talldata$spin_rate, na.rm = TRUE)
kurtosis(talldata$spin_rate, na.rm = TRUE)

hist(shortdata$exit_velocity, main = "", col = "light blue")
hist(talldata$exit_velocity, main = "", col = "royal blue")
boxplot(exit_velocity ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Exit_velocity by Height", outer=TRUE)
skewness(shortdata$exit_velocity, na.rm = TRUE)
kurtosis(shortdata$exit_velocity, na.rm = TRUE)
skewness(talldata$exit_velocity, na.rm = TRUE)
kurtosis(talldata$exit_velocity, na.rm = TRUE)

hist(shortdata$launch_angle, main = "", col = "light blue")
hist(talldata$launch_angle, main = "", col = "royal blue")
boxplot(launch_angle ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Launch_angle by Height", outer=TRUE)
skewness(shortdata$launch_angle, na.rm = TRUE)
kurtosis(shortdata$launch_angle, na.rm = TRUE)
skewness(talldata$launch_angle, na.rm = TRUE)
kurtosis(talldata$launch_angle, na.rm = TRUE)

hist(shortdata$ba, main = "", col = "light blue")
hist(talldata$ba, main = "", col = "royal blue")
boxplot(ba ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Batting average by Height", outer=TRUE)
skewness(shortdata$ba)
kurtosis(shortdata$ba)
skewness(talldata$ba)
kurtosis(talldata$ba)

hist(shortdata$babip, main = "", col = "light blue")
hist(talldata$babip, main = "", col = "royal blue")
boxplot(babip ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("BABIP by Height", outer=TRUE)
skewness(shortdata$babip)
kurtosis(shortdata$babip)
skewness(talldata$babip)
kurtosis(talldata$babip)

hist(shortdata$slg, main = "", col = "light blue")
hist(talldata$slg, main = "", col = "royal blue")
boxplot(slg ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Slugging percentage by Height", outer=TRUE)
skewness(shortdata$slg)
kurtosis(shortdata$slg)
skewness(talldata$slg)
kurtosis(talldata$slg)

hist(shortdata$iso, main = "", col = "light blue")
hist(talldata$iso, main = "", col = "royal blue")
boxplot(iso ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Isolated power by Height", outer=TRUE)
skewness(shortdata$iso)
kurtosis(shortdata$iso)
skewness(talldata$iso)
kurtosis(talldata$iso)

# Filter out outliers based on minimum number of 50 at bats
mydata <- filter(mydata, abs > 50)
talldata <- filter(mydata, tall_short == "tall")
shortdata <- filter(mydata, tall_short == "short")
# 216 observations removed; 1432 remaining 721 tall 711 short

# Check exploratory histograms and normality assessment again
hist(shortdata$velocity, main = "", col = "light blue")
hist(talldata$velocity, main = "", col = "royal blue")
boxplot(velocity ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Velocity by Height", outer=TRUE)
skewness(shortdata$velocity)
kurtosis(shortdata$velocity)
skewness(talldata$velocity)
kurtosis(talldata$velocity)

hist(shortdata$effective_speed, main = "", col = "light blue")
hist(talldata$effective_speed, main = "", col = "royal blue")
boxplot(effective_speed ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Effective_speed by Height", outer=TRUE)
skewness(shortdata$effective_speed, na.rm = TRUE)
kurtosis(shortdata$effective_speed, na.rm = TRUE)
skewness(talldata$effective_speed, na.rm = TRUE)
kurtosis(talldata$effective_speed, na.rm = TRUE)

hist(shortdata$spin_rate, main = "", col = "light blue")
hist(talldata$spin_rate, main = "", col = "royal blue")
boxplot(spin_rate ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Spin_rate by Height", outer=TRUE)
skewness(shortdata$spin_rate, na.rm = TRUE)
kurtosis(shortdata$spin_rate, na.rm = TRUE)
skewness(talldata$spin_rate, na.rm = TRUE)
kurtosis(talldata$spin_rate, na.rm = TRUE)

hist(shortdata$exit_velocity, main = "", col = "light blue")
hist(talldata$exit_velocity, main = "", col = "royal blue")
boxplot(exit_velocity ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Exit_velocity by Height", outer=TRUE)
skewness(shortdata$exit_velocity, na.rm = TRUE)
kurtosis(shortdata$exit_velocity, na.rm = TRUE)
skewness(talldata$exit_velocity, na.rm = TRUE)
kurtosis(talldata$exit_velocity, na.rm = TRUE)

hist(shortdata$launch_angle, main = "", col = "light blue")
hist(talldata$launch_angle, main = "", col = "royal blue")
boxplot(launch_angle ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Launch_angle by Height", outer=TRUE)
skewness(shortdata$launch_angle, na.rm = TRUE)
kurtosis(shortdata$launch_angle, na.rm = TRUE)
skewness(talldata$launch_angle, na.rm = TRUE)
kurtosis(talldata$launch_angle, na.rm = TRUE)

hist(shortdata$ba, main = "", col = "light blue")
hist(talldata$ba, main = "", col = "royal blue")
boxplot(ba ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Batting average by Height", outer=TRUE)
skewness(shortdata$ba)
kurtosis(shortdata$ba)
skewness(talldata$ba)
kurtosis(talldata$ba)

hist(shortdata$babip, main = "", col = "light blue")
hist(talldata$babip, main = "", col = "royal blue")
boxplot(babip ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("BABIP by Height", outer=TRUE)
skewness(shortdata$babip)
kurtosis(shortdata$babip)
skewness(talldata$babip)
kurtosis(talldata$babip)

hist(shortdata$slg, main = "", col = "light blue")
hist(talldata$slg, main = "", col = "royal blue")
boxplot(slg ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Slugging percentage by Height", outer=TRUE)
skewness(shortdata$slg)
kurtosis(shortdata$slg)
skewness(talldata$slg)
kurtosis(talldata$slg)

hist(shortdata$iso, main = "", col = "light blue")
hist(talldata$iso, main = "", col = "royal blue")
boxplot(iso ~ tall_short, mydata, main = "", col = c("light blue","royal blue"))
title("Isolated power by Height", outer=TRUE)
skewness(shortdata$iso)
kurtosis(shortdata$iso)
skewness(talldata$iso)
kurtosis(talldata$iso)
# At this point data looks okay to move forward with t-tests

par(mfrow = c(1,1))

# Perform individual t-tests to determine if differences exist between tall and short pitchers
t.test(velocity ~ tall_short, mydata) # t = -5.0923, df = 1424.6, p-value = 4.01e-07
t.test(effective_speed ~ tall_short, mydata) # t = -4.1286, df = 725.51, p-value = 4.074e-05
t.test(spin_rate ~ tall_short, mydata) # t = -1.6807, df = 722.6, p-value = 0.09326
t.test(exit_velocity ~ tall_short, mydata) # t = -1.0377, df = 733.99, p-value = 0.2997
t.test(launch_angle ~ tall_short, mydata) # t = 0.061186, df = 725.95, p-value = 0.9512
t.test(ba ~ tall_short, mydata) # t = 0.086803, df = 1425.9, p-value = 0.9308
t.test(babip ~ tall_short, mydata) # t = -1.1367, df = 1428.9, p-value = 0.2559
t.test(slg ~ tall_short, mydata) # t = 0.41863, df = 1429.8, p-value = 0.6755
t.test(iso ~ tall_short, mydata) # t = 0.56802, df = 1428.5, p-value = 0.5701

# For the sake of throroughness, checking the non-parametric Wilcoxon rank sum test
# Produced identical results, with only statistically significant differences found on velocity and effective_speed
wilcox.test(velocity ~ tall_short, mydata) # p-value = 3.089e-07
wilcox.test(effective_speed ~ tall_short, mydata) # p-value = 3.235e-05
wilcox.test(spin_rate ~ tall_short, mydata) # p-value = 0.1114
wilcox.test(exit_velocity ~ tall_short, mydata) # p-value = 0.449
wilcox.test(launch_angle ~ tall_short, mydata) # p-value = 0.712
wilcox.test(ba ~ tall_short, mydata) # p-value = 0.7889
wilcox.test(babip ~ tall_short, mydata) # p-value = 0.2189
wilcox.test(slg ~ tall_short, mydata) # p-value = 0.325
wilcox.test(iso ~ tall_short, mydata) # p-value = 0.1523

# Save datasets for future use
write.csv(mydata, file = "assignment2_data.csv")

# End