# Chris Anderson
# PREDICT 455 Data Viz Section 56
# Assignment 4: Visualizing Maps

library(sp)  # spatial analysis in R
library(maptools)  # utilities for working with shapefiles and map projection
library(ggplot2)  # plotting utilities
library(colorspace)  # color functions like HLS() for hue, luminance, and saturation/chroma
library(maps)
library(mapproj)
library(Cairo)

gpclibPermit()  # permit use of non-commercial package gpclib

### DATA PREP ###
# read in MLB draft data and state abbreviations list
draft <- read.csv("mlbdraft2016.csv", header = T, sep = ",")
abbr <- read.csv("stateabbr.csv", header = T, sep = ",")

# merge data sets to get state name on draft data
temp <- aggregate(PICK ~ ST, data = draft, length)
# 36 obs. dropped due to players drafted in Canada and Puerto Rico

# use aggregate function to get number of players drafted per state
mapdf <- merge(x = temp, y = abbr, by = "ST", sort = TRUE, all.y = TRUE)
mapdf <- data.frame(mapdf$State, mapdf$PICK)
colnames(mapdf) <- c("state", "players_drafted")
mapdf$state <- as.character(mapdf$state)
mapdf[is.na(mapdf)] <- 0

### CREATE CHOROPLETH MAP ###
# create clean theme (from Chang chapter 13)
theme_clean <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0,0,0,0), "lines"),
      complete = TRUE)
} # end of theme_clean function

load("USA_adm1.RData")  # US with states shapefile from http://www.gadm.org/
print(ls())  # shows one object named gadm
##print(str(gadm))  # class 'SpatialPolygonsDataFrame'... polygons needed for maps

gadm.data.frame <- fortify(gadm)
# set id as factor with the state name from the gadm object
gadm.data.frame$state <- factor(gadm.data.frame$id, 
                                levels = 1:length(gadm$NAME_1), labels = gadm$NAME_1) 
# character string for state... not factor
gadm.data.frame$state <- as.character(gadm.data.frame$state)
gadm.data.frame <- subset(gadm.data.frame, state != "District of Columbia")

# check that the state names match up between the two data frames
if(!setequal(unique(mapdf$state), unique(gadm.data.frame$state))) {
  cat("\n\nState names from input data:\n\n")
  print(sort(unique(mapdf$state)))
  cat("\n\nState names from GIS database:\n\n")
  print(sort(unique(gadm.data.frame$state)))
  cat("\n\n")
  stop("\n\nExecution terminated")  
}

# merge the MLB draft data with the map data
combined.data.frame <- merge(gadm.data.frame, mapdf, by = "state")

# select polygons within the continental United States
us.data.frame <- subset(combined.data.frame,
                        subset = ((long >= -124.7625) & (long <= -66.9326) &
                                    (lat >= 24.5210) & (lat <= 49.3845)))

# drop the polygons for the holes... lakes...
selected.us.data.frame <- subset(us.data.frame, subset = (!hole))

# print the map to an object
us.map.object <- ggplot(data = selected.us.data.frame, aes(map_id = id, x = long, y = lat, fill = players_drafted)) + 
  geom_map(map = selected.us.data.frame, colour = "black") + 
  coord_map("albers", lat0 = 24.5210, lat1 = 49.3845) + 
  scale_fill_gradient2(low = hex(HLS(12,0.6,0.9)), 
                       mid = "gray90", 
                       high = hex(HLS(225,0.6,0.9)), 
                       midpoint = median(mapdf$players_drafted)) +
  theme_clean() + ggtitle("2016 MLB draft picks by state")

# print the map to the screen (long run time... be patient)
print(us.map.object)  

# put map with no grid lines into a pdf file
pdf(file = "plot_map_draft_picks_by_state.pdf", width = 11, height = 8.5)
print(us.map.object)
dev.off()

# put map with no grid lines into an svg file
svg(file = "plot_map_draft_picks_by_state.svg", width = 11, height = 8.5)
print(us.map.object)
dev.off()


### END ###