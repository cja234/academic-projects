### PREDICT 455 Section 56 ###
# The Show: R Code

# Read in historical MLB database and calculate each team's wins rank for each season
mlb <- read.csv("major_league_baseball_data_v4.csv", header = T, sep = ",")
temp <- subset(mlb, year > 1997)
temp <- transform(temp, 
          wins.rank = ave(reg_season_wins, year, 
                          FUN = function(x) rank(-x, ties.method = "first")))

# Read in MLB payroll data and calculate each team's payroll rank for each season
temp1 <- read.csv("MLB_Payroll_1998-2016.csv", header = T, sep = ",")
temp2 <- merge(temp1, temp, by = "uniqueID", all = TRUE)
mydata <- subset(temp2, year < 2016)
mydata <- data.frame(mydata$Year, mydata$teamID.x, mydata$franchID.x, mydata$Rank.x, mydata$Payroll, mydata$Avg.Salary,
                     mydata$reg_season_wins, mydata$wins.rank, mydata$playoff_qualification, mydata$DivWin, mydata$WCWin, mydata$LgWin, mydata$WSWin)
colnames(mydata) <- c("year", "team_id", "franchID", "payroll_rank", "payroll", "avg_salary", "wins", "win_rank", "playoffs", 
                      "division", "wildcard", "pennant", "world_series")

# Export transformed data for use in Tableau
write.csv(mydata, file = "tableau_data.csv")

# Calculate the variance of wins, mean payoll, and mean attendance from historical MLB database
temp1 <- subset(temp1, Year < 2016)
df <- aggregate(reg_season_wins ~ year, data = temp, var)
df1 <- aggregate(attendance ~ year, data = temp, mean)
df2 <- aggregate(Payroll ~ Year, data = temp1, mean)
mydf <- data.frame(df, df2$Payroll, df1$attendance)
colnames(mydf) <- c("year", "variance_of_wins", "mean_payroll", "mean_attendance")

# Export transformed data for use in Tableau
write.csv(mydf, file = "tableau_data2.csv")

# Read in MLB CBA database and combine with salary and variance of wins data
temp2 <- subset(mlb, year > 1960)
temp3 <- read.csv("MLB_cba_db.csv", header = T, sep = ",")
temp4 <- read.csv("MLB_salary_stats_1967-2016.csv", header = T, sep = ",")
df3 <- aggregate(reg_season_wins ~ year, data = temp2, var)
df4 <- merge(df3, temp3, by = "year", all = TRUE)
df4 <- merge(df4, temp4, by.x = "year", by.y = "Year", all = TRUE)
df4[is.na(df4)] <- 0

# Export transformed data for use in Tableau
write.csv(df4, file = "tableau_data3.csv")

### END ###
