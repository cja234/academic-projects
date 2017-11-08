# Read in PITCHf/x data downloaded from MLB Statcast search tool
mydata <- read.csv("arrieta_data.csv", header = T, sep = ",", stringsAsFactors = FALSE)
mydata2 <- read.csv("arrieta_data2.csv", header = T, sep = ",", stringsAsFactors = FALSE)

# Convert variables z0 and x0 (aka vertical release point and horizontal release point) to numeric 
mydata$z0 <- as.numeric(as.character(mydata$z0))
mydata$x0 <- as.numeric(as.character(mydata$x0))

# Calculate mean release points per year
mytable <- aggregate(z0 ~ game_year, data = mydata, mean)
mytable <- mytable[order(mytable$game_year, decreasing = TRUE),]
mytable2 <- aggregate(x0 ~ game_year, data = mydata, mean)
mytable2 <- mytable2[order(mytable2$game_year, decreasing = TRUE),]

# Combine table with other stats collected from MLB Statcast search tool
mytable <- cbind(mytable, mytable2$x0, mydata2)

# Write to csv for use in Tableau
write.csv(mytable, file = "arrieta_table.csv")

# END