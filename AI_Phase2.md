# AI_Phase2
Sentiment Analysis for Marketing based on the dataset of US Airline Sentiment data from Twitter


#import necessary libraries 
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"),"../input/database.sqlite")
library(dplyr)
tables <- dbGetQuery(db, "SELECT Name FROM sqlite_master WHERE type='table'")
colnames(tables) <- c("Name")
tables <- tables %>%
          rowwise() %>%
          mutate(RowCount=dbGetQuery(db, paste0("SELECT COUNT(*) RowCount FROM ", Name))$RowCount[1])
print.table(tables)
print.table(dbGetQuery(db, "SELECT * FROM Tweets LIMIT 6"))
library(ggvis)
dbGetQuery(db, "
SELECT airline Airline,
       airline_sentiment Sentiment,
       COUNT(airline) NumTweets
FROM Tweets
GROUP BY airline,
         airline_sentiment") %>%
  ggvis(~Airline, ~NumTweets, fill=~Sentiment) %>%
  layer_bars() #fill:="#20beff")
print.table(dbGetQuery(db, "
SELECT airline,
       negativereason,
       COUNT(negativereason)
FROM Tweets
GROUP BY airline,
         negativereason
ORDER BY COUNT(negativereason) DESC"))
library(tm)
library(wordcloud)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[150]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}

makeWordCloud(dbGetQuery(db, "SELECT text FROM Tweets")$text)
