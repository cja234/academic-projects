# Bring relevant packages into workspace
library(readr)
library(tm)  
library(wordcloud)
library(stringr)

# Read in Hilary Clinton transcript and convert all to lowercase letters
hrc.text <- tolower(scan("clinton.txt", what = "char", sep = "\n"))

# Replace common contractions with whole words
hrc.text <- str_replace_all(hrc.text, "can't", "cannot")
hrc.text <- str_replace_all(hrc.text, "won't", "will not")
hrc.text <- str_replace_all(hrc.text, "don't", "do not")
hrc.text <- str_replace_all(hrc.text, "'s", "")
hrc.text <- str_replace_all(hrc.text, "n't", " not")
hrc.text <- str_replace_all(hrc.text, "'ve", " have")
hrc.text <- str_replace_all(hrc.text, "'ll", " will")
hrc.text <- str_replace_all(hrc.text, "'re", " are")
hrc.text <- str_replace_all(hrc.text, "'m", " am")
hrc.text <- str_replace_all(hrc.text, "'d", " would")

# Consider whole words only and create vector
hrc.text.words <- unlist(strsplit(hrc.text, "\\W"))
hrc.text.vector <- hrc.text.words[which(nchar(hrc.text.words) > 0)]

# Create frequency table from word vector
hrc.freq.list <- table(hrc.text.vector)
hrc.sorted.freq.list<-sort(hrc.freq.list, decreasing=TRUE)
hrc.sorted.table<-paste(names(hrc.sorted.freq.list), hrc.sorted.freq.list)

# Print to .csv file for use in Tableau
cat("Word FREQ", hrc.sorted.table, file="clinton_word_freq.csv", sep = "\n")

# Repeat process for Donald Trump transcript
# Read in file and convert all to lowercase letters
djt.text <- tolower(scan("trump.txt", what = "char", sep = "\n"))

# Replace common contractions with whole words
djt.text <- str_replace_all(djt.text, "can't", "cannot")
djt.text <- str_replace_all(djt.text, "won't", "will not")
djt.text <- str_replace_all(djt.text, "don't", "do not")
djt.text <- str_replace_all(djt.text, "'s", "")
djt.text <- str_replace_all(djt.text, "n't", " not")
djt.text <- str_replace_all(djt.text, "'ve", " have")
djt.text <- str_replace_all(djt.text, "'ll", " will")
djt.text <- str_replace_all(djt.text, "'re", " are")
djt.text <- str_replace_all(djt.text, "'m", " am")
djt.text <- str_replace_all(djt.text, "'d", " would")

# Consider whole words only and create vector
djt.text.words <- unlist(strsplit(djt.text, "\\W"))
djt.text.vector <- djt.text.words[which(nchar(djt.text.words) > 0)]

# Create frequency table from word vector
djt.freq.list <- table(djt.text.vector)
djt.sorted.freq.list<-sort(djt.freq.list, decreasing=TRUE)
djt.sorted.table<-paste(names(djt.sorted.freq.list), djt.sorted.freq.list)

# Print to .csv file for use in Tableau
cat("Word FREQ", djt.sorted.table, file="trump_word_freq.csv", sep = "\n")

# End