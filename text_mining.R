library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
#Ler o arquivo publication
public <- jsonlite::fromJSON("comp_aplicada/publication.json")

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("scripts/elattes.ls2df.R")


public.periodico.df <- pub.ls2df(public, 1) #artigos
str(public.periodico.df)

docs <- Corpus(VectorSource(public.periodico.df$titulo))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("wwwelseviercom", "wwwworg", "dtdquot")) 

inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(dtm, terms = "network", corlimit = 0.3)
findAssocs(dtm, terms = "data", corlimit = 0.3)
findAssocs(dtm, terms = "classfication", corlimit = 0.3)

#Ler o arquivo publication
public <- jsonlite::fromJSON("cic/publication.json")

public.periodico.df <- pub.ls2df(public, 1) #artigos
str(public.periodico.df)

docs <- Corpus(VectorSource(public.periodico.df$titulo))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("using", "para"))

inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 11,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(dtm, terms = "network", corlimit = 0.3)


