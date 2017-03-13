#loading the input file
library(readxl)
twitter <- read_excel(file.choose())


#creating a corpus
library(tm)
tweets <- Corpus(VectorSource(twitter))
head(tweets)

#transforming the data
tweets <- tm_map(tweets,removeNumbers)
tweets <- tm_map(tweets,removePunctuation)
tweets <- tm_map(tweets,stripWhitespace)
tweets <- tm_map(tweets,content_transformer(tolower))
tweets <- tm_map(tweets,removeWords,stopwords("english"))


#stemmming the document
library(SnowballC)
tweets <- tm_map(tweets,stemDocument)
tweets <- tm_map(tweets,removeWords,
                 c("https:\\w+","Twitter","twitter","#","RT","http:\\w+","oscar","@","via","eu "))


#create a DTM
library(syuzhet)
dtm <- TermDocumentMatrix(tweets)

#storing it in a matrix
dtm_matrix <- as.matrix(dtm)

dtm_rowsum <- rowSums(dtm_matrix)

dtm_count <- data.frame(word=names(dtm_rowsum),freq=dtm_rowsum)


#wordcloud

library(wordcloud)
library(RColorBrewer)

png("Tweets.png",width =720 ,height=1080)
wordcloud(dtm_count$word,dtm_count$freq,random.order = F,
          min.freq = 70,max.words = 500,rot.per = 0.35, colors=brewer.pal(10, "Paired"))
title("Oscars 2017 Word Cloud - Madhur Inani")
dev.off()


#sentiment analysis


library("syuzhet")

frame<- data.frame(twitter)
frame <- sub("[^0-9A-Za-z///' ]", "",frame)
count <- get_nrc_sentiment(frame)
head(count)
senti_df <- data.frame(t(count))
senti_df


#tansform and clean
names(senti_df[1]) <- "count"

head(senti_df,10)

senti_df <- cbind("sentiment"=rownames(senti_df),senti_df)


row.names(senti_df) <- NULL
senti_df <- senti_df[1:8,]
senti_df

#final visualization
library(ggplot2)
ggplot(data=senti_df,aes(x=sentiment,y=t.count.,fill=sentiment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = t.count.),hjust=1) + coord_flip() + 
  ggtitle("#Oscars2017 Sentiment Analysis") 
  



# graph for top devices used for tweet
Tweet_devices <- read.csv(file.choose())

library(dplyr)

Device <- Tweet_devices%>%
  group_by(App)%>%
  summarise(App_count=n())%>%
  arrange(desc(App_count))%>%
  filter(App_count>400)
  
colnames(Device)[2] <- "Count"


ggplot(data=Device,aes(x=App,y=Count)) +
  geom_point(color="Red") + coord_flip()+ ggtitle("Top Devices") +
  theme_grey() + geom_text(aes(label=Device$Count),size=4,vjust=-0.3)
