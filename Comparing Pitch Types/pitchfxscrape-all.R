# PREDICT 456 Sports Performance Analysis Section 55 Summer 2016
# Christopher Anderson
# Assignment #4
# Last modified August 24, 2016

library(moments)
library(ggplot2)
library(gridExtra)
library(pitchRx)
library(dplyr)
library(Hmisc)
library(RSQLite)
library(XML)
library(psych)

db <- src_sqlite("pitchfx.sqlite3")
files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")

# Download 2008 PITCHf/x data
scrape(start = "2008-01-01", end = "2008-04-30", connect = db$con)
scrape(start = "2008-01-01", end = "2008-04-30", suffix = files, connect = db$con)
scrape(start = "2008-05-01", end = "2008-05-31", connect = db$con)
scrape(start = "2008-05-01", end = "2008-05-31", suffix = files, connect = db$con)
scrape(start = "2008-06-01", end = "2008-06-30", connect = db$con)
scrape(start = "2008-06-01", end = "2008-06-30", suffix = files, connect = db$con)
scrape(start = "2008-07-01", end = "2008-07-31", connect = db$con)
scrape(start = "2008-07-01", end = "2008-07-31", suffix = files, connect = db$con)
scrape(start = "2008-08-01", end = "2008-08-31", connect = db$con)
scrape(start = "2008-08-01", end = "2008-08-31", suffix = files, connect = db$con)
scrape(start = "2008-09-01", end = "2008-09-30", connect = db$con)
scrape(start = "2008-09-01", end = "2008-09-30", suffix = files, connect = db$con)
scrape(start = "2008-10-01", end = "2008-12-31", connect = db$con)
scrape(start = "2008-10-01", end = "2008-12-31", suffix = files, connect = db$con)

# Download 2009 PITCHf/x data
scrape(start = "2009-01-01", end = "2009-04-30", connect = db$con)
scrape(start = "2009-01-01", end = "2009-04-30", suffix = files, connect = db$con)
scrape(start = "2009-05-01", end = "2009-05-31", connect = db$con)
scrape(start = "2009-05-01", end = "2009-05-31", suffix = files, connect = db$con)
scrape(start = "2009-06-01", end = "2009-06-30", connect = db$con)
scrape(start = "2009-06-01", end = "2009-06-30", suffix = files, connect = db$con)
scrape(start = "2009-07-01", end = "2009-07-31", connect = db$con)
scrape(start = "2009-07-01", end = "2009-07-31", suffix = files, connect = db$con)
scrape(start = "2009-08-01", end = "2009-08-31", connect = db$con)
scrape(start = "2009-08-01", end = "2009-08-31", suffix = files, connect = db$con)
scrape(start = "2009-09-01", end = "2009-09-30", connect = db$con)
scrape(start = "2009-09-01", end = "2009-09-30", suffix = files, connect = db$con)
scrape(start = "2009-10-01", end = "2009-12-31", connect = db$con)
scrape(start = "2009-10-01", end = "2009-12-31", suffix = files, connect = db$con)

# Download 2010 PITCHf/x data
scrape(start = "2010-01-01", end = "2010-04-30", connect = db$con)
scrape(start = "2010-01-01", end = "2010-04-30", suffix = files, connect = db$con)
scrape(start = "2010-05-01", end = "2010-05-31", connect = db$con)
scrape(start = "2010-05-01", end = "2010-05-31", suffix = files, connect = db$con)
scrape(start = "2010-06-01", end = "2010-06-30", connect = db$con)
scrape(start = "2010-06-01", end = "2010-06-30", suffix = files, connect = db$con)
scrape(start = "2010-07-01", end = "2010-07-31", connect = db$con)
scrape(start = "2010-07-01", end = "2010-07-31", suffix = files, connect = db$con)
scrape(start = "2010-08-01", end = "2010-08-31", connect = db$con)
scrape(start = "2010-08-01", end = "2010-08-31", suffix = files, connect = db$con)
scrape(start = "2010-09-01", end = "2010-09-30", connect = db$con)
scrape(start = "2010-09-01", end = "2010-09-30", suffix = files, connect = db$con)
scrape(start = "2010-10-01", end = "2010-12-31", connect = db$con)
scrape(start = "2010-10-01", end = "2010-12-31", suffix = files, connect = db$con)

# Download 2011 PITCHf/x data
scrape(start = "2011-01-01", end = "2011-04-30", connect = db$con)
scrape(start = "2011-01-01", end = "2011-04-30", suffix = files, connect = db$con)
scrape(start = "2011-05-01", end = "2011-06-30", connect = db$con)
scrape(start = "2011-05-01", end = "2011-06-30", suffix = files, connect = db$con)
scrape(start = "2011-07-01", end = "2011-08-31", connect = db$con)
scrape(start = "2011-07-01", end = "2011-08-31", suffix = files, connect = db$con)
scrape(start = "2011-09-01", end = "2011-12-31", connect = db$con)
scrape(start = "2011-09-01", end = "2011-12-31", suffix = files, connect = db$con)

# Download 2012 PITCHf/x data
scrape(start = "2012-01-01", end = "2012-05-31", connect = db$con)
scrape(start = "2012-01-01", end = "2012-05-31", suffix = files, connect = db$con)
scrape(start = "2012-06-01", end = "2012-07-31", connect = db$con)
scrape(start = "2012-06-01", end = "2012-07-31", suffix = files, connect = db$con)
scrape(start = "2012-08-01", end = "2012-12-31", connect = db$con)
scrape(start = "2012-08-01", end = "2012-12-31", suffix = files, connect = db$con)

# Download 2013 PITCHf/x data
scrape(start = "2013-01-01", end = "2013-06-30", connect = db$con)
scrape(start = "2013-01-01", end = "2013-06-30", suffix = files, connect = db$con)
scrape(start = "2013-07-01", end = "2013-12-31", connect = db$con)
scrape(start = "2013-07-01", end = "2013-12-31", suffix = files, connect = db$con)

# Download 2014 PITCHf/x data
scrape(start = "2014-01-01", end = "2014-06-30", connect = db$con)
scrape(start = "2014-01-01", end = "2014-06-30", suffix = files, connect = db$con)
scrape(start = "2014-07-01", end = "2014-12-31", connect = db$con)
scrape(start = "2014-07-01", end = "2014-12-31", suffix = files, connect = db$con)

# Download 2015 PITCHf/x data
scrape(start = "2015-01-01", end = "2015-06-30", connect = db$con)
scrape(start = "2015-01-01", end = "2015-06-30", suffix = files, connect = db$con)
scrape(start = "2015-07-01", end = "2015-12-31", connect = db$con)
scrape(start = "2015-07-01", end = "2015-12-31", suffix = files, connect = db$con)

# Download 2016 PITCHf/x data
scrape(start = "2016-01-01", end = "2016-06-30", connect = db$con)
scrape(start = "2016-01-01", end = "2016-06-30", suffix = files, connect = db$con)
scrape(start = "2016-07-01", end = "2016-08-21", connect = db$con)
scrape(start = "2016-07-01", end = "2016-08-21", suffix = files, connect = db$con)


#  Check data
dbListTables(db$con)
df <- dbGetQuery(db$con, 'select * from action')
head(df)
tail(df)




