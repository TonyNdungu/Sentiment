########################################################
# required packages

library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Twitter API Oauth process.
consumer_key <- 'T247avsV5cil8YUYuSYl25yCa'
consumer_secret <- 'W0HPnGJp2XqsLpyhImjOgem29f0dcxIrkDdzO0aicv3nMDbGdy'
access_token <- '367326351-nFSKL8lSzaFxoTKq2N0WYL8YgTimfReoVvBorNDv'
access_secret <- '9dabbWazeHYRz3mHPseTA1O9YBmaMakUJvLgrfKRDigDC'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Step 1: 
# harvest some tweets
some_tweets = searchTwitter("#AGRF2016", n=10000, lang="en")

some_tweets.df = twListToDF(some_tweets)
# View data frame 
View(some_tweets.df)
# Save dataframe into local machine as csv file
write.csv(some_tweets.df, file='C:/Users/Admin/Documents/agric.csv', row.names = F)

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

########################################################

# Step 2: prepare the text for sentiment analysis
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

########################################################

# Step 3: Perform sentiment analysis

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

########################################################

# Step 4: Create data frame with the results and obtain some general statistics

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

########################################################

# Step 5: Do some plots of the obtained results

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of text")
       plot.title = theme_text(size=12))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of text")
       plot.title = theme_text(size=12))

########################################################

# Step 7: Separate the text by emotions and visualize the words with a comparison cloud

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

