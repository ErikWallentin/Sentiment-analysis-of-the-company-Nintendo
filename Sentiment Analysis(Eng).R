#####################################################################################################
# Many companies and organizations benefits greatly from finding out what the public thinks about them. 
# One way to do that is to investigate what people are writing about them on social media. 
# A large social platform, which is actively used by more than 300 million people worldwide, is Twitter.
# An analysis of tweets aimed at companies/organizations through e.g. a given hashtag, can help 
# companies/organizations to get a clearer view of what the public's attitude towards them is. 
# When launching a new product, this analysis method is particularly effective, as it quickly and 
# easily provides feedback from the public about what they think about the given product.

# This type of analysis is called sentiment analysis and the goal of this project is to find out what 
# people write on Twitter regarding the company Nintendo, who just announced that Nintendo of 
# America's president, Reggie Fils-Aime, will retire and be replaced by Doug Bowser. 
# We therefore want to find out about the public's reaction to this announcement and this 
# project will answer the question of what people write about Nintendo and how its attitude towards 
# the company is, through an analysis of 1000 English-speaking tweets that include the Nintendo hashtag.
# The tweets were fetched and saved in a csv-file with help of Twitter's API and the R package "rtweet".

# In order to find out what's written about the company on Twitter, two graphs are presented, 
# a barplot and a wordcloud, containing information about which words most frequently occur among the
# 1000 tweets. Similar words, e.g. run, running, runs etc is categorized under one name with help of a
# method called stemming. This means that some words in the graphs may have slightly different 
# spelling as well as grammatical bending. However, this is a price I think it's worth paying to get 
# the opportunity to merge similar words under one name.

# An analysis of the words presented in the two graphs can provide an acceptable understanding of what
# the public thinks about Nintendo after the presidential change announcement. However, there is a 
# more efficient way to find out if the 1000 tweets can be categorized as positive, negative, fearful,
# etc. by a method that goes through each word of each tweet and gives a score to each word regarding 
# if they were positive, negative, fearful, etc.
# After each word in each tweet is analysed by this method, we can through a barplot visualise for 
# example how many points the "positive" category has accumulated. The method in question can 
# categorize a word into ten different categories, and it's possible for a word to be categorized 
# into more than one category.

# This project is divided into three separated parts where the first is dedicated to preparing the 
# data, since text from social media is often data that needs to be prepared and cleaned thoroughly 
# before the analysis phase. Part two is about creating the two graphs that describe which words 
# occur most frequently in the tweets. Finally, the final part is all about the implementation of our
# sentiment analysis, were the goal is to through these 1000 tweets find out the general attitude 
# towards the company Nintendo.
# The project concludes with a brief comment on the result achieved.
#####################################################################################################


#######################################################################
###########################Prepare the data############################
#######################################################################


# R-package necessary to execute the project.
install.packages("quanteda")
install.packages("wordcloud2")
install.packages("syuzhet")

library(quanteda)
library(wordcloud2)
library(syuzhet)


# Import the csv file named "TweetsNintendo" which contains 1000 tweets.
Data <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = F)


# Remove links in the tweets.
Data$Tweet <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", Data$Tweet)


# Remove the code that represent emojis in the tweets.
Data$Tweet <- gsub("<.*?>", "", Data$Tweet)


# Remove words beginning with "#" or "@" in the tweets.
# If this isn't done, the word #Nintendo would for example appear in each tweet, and the character "@"
# is used as a link to another twitter-user. Words starting with @ are therefore superfluous as they
# don't help us extract any information about what the person who wrote the tweet thinks about the 
# company Nintendo.
Data$Tweet <- gsub("[@#]\\w+ *", "", Data$Tweet)


# Tokenize every word in the tweets, and remove points, symbols, hyphens and words that only consists 
# of numbers.
# In order to use the steaming function, every word needs to become a separate token
Data.tokens <- tokens(Data$Tweet, what = "word", 
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE)


# Convert all tokens to lowercase.
Data.tokens <- tokens_tolower(Data.tokens)


# Remove english "stop-words" in our tweets.
Data.tokens <- tokens_select(Data.tokens, stopwords(), 
                             selection = "remove")


# Perform stemming on our tokens.
Data.tokens <- tokens_wordstem(Data.tokens, language = "english")


# Create a document-feature matrix and then convert it to a matrix.
# When converting, the transpose function is used so that the columns contains the tweets and the rows 
# contains the words.
dfm <- dfm(Data.tokens, tolower = FALSE)
dfm <- t(as.matrix(dfm))


#######################################################################
###Create two graphs containing the most frequently occurring words####
#######################################################################


# Create a barplot that illustrates which words occur most frequently in our 1000 tweets. 
# Only words that occur at least 40 times are shown in the graph.
words <- rowSums(dfm)
words <- subset(words, words >= 40)
barplot(words, las = 2, main = 'Most frequently occurring words', col = rainbow(40), ylim = c(0,350))


# Create a "wordcloud" that illustrates which words occur most frequently in our 1000 tweets. 
# By hovering the mouse over a word in the graph, the number of times the given word appears in the 
# 1000 tweets is shown.
words_sorted <- sort(rowSums(dfm), decreasing = TRUE)
words_df <- data.frame(names(words_sorted), words_sorted)
colnames(words_df) <- c('word', 'freq')
head(words_df)
cloud <- wordcloud2(words_df, size = 0.7, shape = 'circle', minSize = 2, rotateRatio = 0.30)
cloud


#######################################################################
##########################Sentiment Analysis###########################
#######################################################################


# Perform sentiment analysis on our 1000 tweets.
tweets <- iconv(Data$Tweet)
Sentiment_Scores <- get_nrc_sentiment(tweets)


# Create a barplot that reports the result of our sentiment analysis.
par(mar=c(6,4,4,2))
barplot(colSums(Sentiment_Scores), las = 2, col = rainbow(10), 
        main = 'Sentiment analysis of #Nintendo tweets', ylab = 'Count', ylim = c(0,1000))

#####################################################################################################
# The result of the sentiment analysis shows that the attitude towards the company Nintendo is mostly 
# positive. This is in line with what one might expect after inspection of the two graphs containing 
# information about the words that occur most frequently in the 1000 tweets. We can see that words
# that are positively charged occur to a greater extent than negatively charged words.
# An explanation of why the bars "positive" and "anticipation" are the greatest in the graph could be 
# explained by the fact that many may thank the old president, Reggie Fils-Aime, for a well done work 
# at Nintendo of America, and that many are excited about what the new president, Doug Bowser, can 
# accomplish.
#####################################################################################################