library(twitteR)
# Twitter API Oauth process.
consumer_key <- 'T247avsV5cil8YUYuSYl25yCa'
consumer_secret <- 'W0HPnGJp2XqsLpyhImjOgem29f0dcxIrkDdzO0aicv3nMDbGdy'
access_token <- '367326351-nFSKL8lSzaFxoTKq2N0WYL8YgTimfReoVvBorNDv'
access_secret <- '9dabbWazeHYRz3mHPseTA1O9YBmaMakUJvLgrfKRDigDC'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

delta.tweets = searchTwitter('@delta', n=500)

#see what we got in return
length(delta.tweets)
class(delta.tweets)

library(plyr)

#Extract text
delta.text = laply(delta.tweets, function(t) t$getText())
length(delta.text)

head(delta.text, 5)

# Step 2: We need to import the files containing the positive and negative words
pos = scan('/Users/tony/Documents/opinion lexicon/positive_words.txt', what='character', comment.char=';')
neg = scan('/Users/tony/Documents/opinion lexicon/negative_words.txt', what='character', comment.char=';')


#Add score.sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sample = c("You're awesome and I love you",
           "I hate and hate and hate. So angry. Die!",
           "Impressed and amazed: you are peerless in your achievement of
           unparallelled mediocrity.")

result = score.sentiment(sample, pos, neg)
View(result)
class(result)
result$score

delta.scores = score.sentiment(delta.text, pos, neg, .progress='text')

delta.scores$airline = 'Delta'

hist(delta.scores$score)

library(ggplot2)
qplot(delta.scores$score)

library(RColorBrewer)
#all palette available from RColorBrewer
display.brewer.all()
#we will select the first 4 colors in the Set1 palette
cols<-brewer.pal(n=4,name="Set1")

