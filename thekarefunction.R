###############################################
# Step 1: Load all the necessary libraries, Oauth and etc
require(twitteR)
require(plyr)
require(stringr)
require(RCurl)
require(ROAuth)
require(ggplot2)

# Step 2: We need to import the files containing the positive and negative words
pos = scan('/Users/tony//Documents/opinion lexicon/positive_words.txt', what='character', comment.char=';')
neg = scan('/Users/tony/Documents/opinion lexicon/negative_words.txt', what='character', comment.char=';')

#######################################################################################################

#Step 3: Define function score.sentiment
# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
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
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

##################################################################################

# Step 4: Twitter API Oauth process.
consumer_key <- 'T247avsV5cil8YUYuSYl25yCa'
consumer_secret <- 'W0HPnGJp2XqsLpyhImjOgem29f0dcxIrkDdzO0aicv3nMDbGdy'
access_token <- '367326351-nFSKL8lSzaFxoTKq2N0WYL8YgTimfReoVvBorNDv'
access_secret <- '9dabbWazeHYRz3mHPseTA1O9YBmaMakUJvLgrfKRDigDC'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

########################################################################################################

#Step 5: Let's harvest tweets talking about wine, beer, coffee, and soda 
# tweets with drinks
wine_tweets = searchTwitter("wine", n=500, lang="en")
beer_tweets = searchTwitter("beer", n=500, lang="en")
coffee_tweets = searchTwitter("coffee", n=500, lang="en")
soda_tweets = searchTwitter("soda", n=500, lang="en")

# get text
wine_txt = sapply(wine_tweets, function(x) x$getText())
beer_txt = sapply(beer_tweets, function(x) x$getText())
coffee_txt = sapply(coffee_tweets, function(x) x$getText())
soda_txt = sapply(soda_tweets, function(x) x$getText())

# how many tweets of each drink
nd = c(length(wine_txt), length(beer_txt), length(coffee_txt), length(soda_txt))

# join texts
drinks = as.list(c(wine_txt, beer_txt, coffee_txt, soda_txt) )

################################################################################

# Step 6: Apply score.sentiment and calculate more result
# apply function score.sentiment
scores = score.sentiment(drinks, pos, neg, .progress='text')
scores = score.sentiment("so how negative is this", pos, neg, .progress='text')

scr_df = lapply(drinks, score.sentiment, pos,neg,.progress='text')

scr_df2 = rbind(scr_df)

= rbind(lapply(drinks, score.sentiment, pos,neg,.progress='text'))

scores 

# add variables to data frame
scores$drink = factor(rep(c("wine", "beer", "coffee", "soda"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )

################################################################################

#Step 7(optional): Define the colors for the barplot
# colors
cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) = c("beer", "coffee", "soda", "wine")

################################################################################

# Step 8: Make some barplots
# As you can tell, wine gets the highest sentiment score, while soda the lowest one
# barplot of average score
meanscore = tapply(scores$score, scores$drink, mean)
df = data.frame(drink=names(meanscore), meanscore=meanscore)
df$drinks <- reorder(df$drink, df$meanscore)

ggplot(df, aes(y=meanscore, x=drinks, fill=drinks)) +  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[order(df$meanscore)]) 
