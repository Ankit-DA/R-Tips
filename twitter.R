##
setwd("C:/Users/an011ag/Documents")
getwd()
##
##
# First let us create some functions that will be used repeatedly in our analysis
#
##
# This function will convert the twitter output into a dataframe
##
#
TweetFrame<-function(searchterm,maxtweets)
{
  
  
  tweetlist<-searchTwitter(searchterm,maxtweets,cainfo="cacert.pem")
  #
  # Let us now convert the tweet list received from Twitter into a Data Frame
  #
  tweetdf<-twListToDF(tweetlist)
  #
  #Another way to do this is 
  # tweetdf <- do.call("rbind",lapply(tweetlist,as.data.frame))
  #
  # Let us now return the tweet dataframe in arrival order
  #
  return(tweetdf[order(as.integer(tweetdf$created)),])
}
#
#
# This function will check the presence of a R Package in the workspace
#
EnsurePackage<-function(x)
{
  x<-as.character(x)
  #
  if (!require(x,character.only=TRUE))   # character.only tells require that x should have only character string
  {
    install.packages(pkgs=x,dependencies = TRUE)
    require(x,character.only=TRUE)
  }
}
#
#
# This function will add all the required R packages , for twitter analysis, to the Workspace
#
PrepareTwitter<-function()
{
  EnsurePackage("ROAuth")
  EnsurePackage("twitteR")
  EnsurePackage("wordcloud")
  EnsurePackage("tm")
  EnsurePackage("ggplot2")
  EnsurePackage("RColorBrewer")
}
#
#
# This function will clean the tweet text by removing URL, # tags, Redundant Spaces etc
CleanTweets<-function(tweetdf)
{
  # Lets us clean the tweet text by removing URL, # tags, Redundant Spaces etc
  tweettext<-tweetdf$text
  #
  # Remove redundant spaces
  #
  tweettext<-gsub("  "," ",tweettext)
  #
  # remove retweet entities
  #
  tweettext = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweettext)
  #
  # remove Atpeople
  #
  tweettext = gsub("@\\w+", "", tweettext)
  #
  # remove punctuation symbols
  #
  tweettext = gsub("[[:punct:]]", "", tweettext)
  #
  # remove numbers
  #
  tweettext = gsub("[[:digit:]]", "", tweettext)
  #
  # remove links
  #
  tweettext = gsub("http\\w+", "", tweettext)
  #
  #
  # Same could also be achieved using stringr package. Lets see how we can do that using stringr.
  #
  #require(stringr)
  #
  # Remove redundant spaces
  # tweettext = str_replace_all(tweettext."  "," ")
  #
  # Remove URLs
  # tweettext <- str_replace_all(tweettext,"http:\\w+","")
  # 
  # Remove retweet headers (only one per tweet so use str_replace in place of str_replace_all)
  # tweettext<- str_replace(tweettext,"RT @[a-z,A-Z,0-9]{8}","")
  #
  # Remove hashtags
  # tweettext<- str_replace_all(tweettext,"#[a-z,A-Z,0-9]*","")
  #
  # Remove reference to other screennames
  # tweettext<- str_replace_all(tweettext,"@[a-z,A-Z,0-9]*","")
  #
  tweetdf$text<-tweettext
  #
  return(tweetdf)
}
#
#
# This function will clean the text by removing stopwords, converting all characters to lowercase etc
#
# We will be using tm_map function of tm package for this purpose
# tm package is specifically used for Text Mining purpose
#
#
GetCleanCorpus<-function(tweetdf)
{
  
  # 
  # Create corpus (Corpus is nothing but a custom class like Java classes or C structure)
  tweet_corpus = Corpus(VectorSource(tweetdf$text))
  #
  # convert to lower case
  #
  tweet_corpus = tm_map(tweet_corpus, tolower)
  #
  # remove stoprwords (stopwords like a, at, and , the etc)
  #
  tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("english")))
  #
  # remove extra white-spaces (though we already did this in CleanTweets)
  #
  tweet_corpus = tm_map(tweet_corpus, stripWhitespace)
  #
  return(tweet_corpus)
}
#
#
# This function will do clustering and prepare data for plotting Dendrogram
#
#
CreateDendrogram<-function(tweet_corpus)
{
  # Create term-document matrix from the corpus using TermDocumentMatrix function of tm package
  tdm = TermDocumentMatrix(tweet_corpus)
  #
  # convert as matrix
  #
  m = as.matrix(tdm)
  #
  # For convenience and to avoid cluttering, let's keep those words that have a frequency > 80 percentile
  # Or in other words that are less than 10% sparse
  #
  # remove sparse terms (word frequency > 90% percentile)
  wf = rowSums(m)
  m1 = m[wf>quantile(wf,probs=0.9), ]
  #
  # remove columns with all zeros
  m1 = m1[,colSums(m1)!=0]
  #
  # for convenience, every matrix entry must be binary (0 or 1) so convert any calue greater than 1 to 1
  # This is sone to make sure that if any term is repeated in a tweet, still we count it only once
  m1[m1 > 1] = 1
  #
  # Lets keep exploring by applying a cluster analysis and discover more about groups of words
  #
  # distance matrix with binary distance
  m1dist = dist(m1, method="binary")
  #
  # cluster with ward method
  clus1 = hclust(m1dist, method="ward")
  #
  # plot dendrogram
  plot(clus1, cex=0.5)
  #
  #
}
#
#
### Well we have created the required functions now ###
#
### Its the time to play by retrieving date for some hashtags and generate their Dendograms ###
#
### As the political fever is high in India these days, lets see what is being spoken about the prime-ministerial candidates in India ###
#
### Vroooooooommmmmm.....
#
#
PrepareTwitter()
# In an earlier post 'Extract Tweet Information using R', we saw how to establish authentication with Twitter API by extracting Key and Secret and stored the authentication file in our system. Here we will load that file to avoid doing authentication again
if(file.exists("credentials.RData"))  # This was the credential File we saved
{
  load(file = "credentials.RData")
  registerTwitterOAuth(Credentials)
} else {
  Credentials<- OAuthFactory$new(consumerKey='key goes here',
                                 consumerSecret='secret goes here',
                                 requestURL='https://api.twitter.com/oauth/request_token',
                                 accessURL='https://api.twitter.com/oauth/access_token',
                                 authURL='https://api.twitter.com/oauth/authorize')
  Credentials$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(Credentials, file="credentials.RData")
  registerTwitterOAuth(Credentials)    #Now that we have the credential, we register it for this session
}
#
#
# Lets begin with BJP first
#
BJP_tweetdf<-TweetFrame("NarendraModi",1000)
BJP_tweetdf<-CleanTweets(BJP_tweetdf)
BJP_tweet_corpus<-GetCleanCorpus(BJP_tweetdf)
CreateDendrogram(BJP_tweet_corpus)
#
#
# Looks interesting !
#
# Lets see what Congress party says
#
Cong_tweetdf<-TweetFrame("RahulGandhi",1000)
Cong_tweetdf<-CleanTweets(Cong_tweetdf)
Cong_tweet_corpus<-GetCleanCorpus(Cong_tweetdf)
CreateDendrogram(Cong_tweet_corpus)
#
#
# Finally lets have a quick look at the etymology used by emerging party AAP
#
#
AAP_tweetdf<-TweetFrame("ArvindKejriwal",1000)
AAP_tweetdf<-CleanTweets(AAP_tweetdf)
AAP_tweet_corpus<-GetCleanCorpus(AAP_tweetdf)
CreateDendrogram(AAP_tweet_corpus)
#
#
#
# Well sometimes Dendrogram doesn't look too promising. Nevertheless, lets try to create some better visual by creating wordcloud
#
# We already have required data with us so just Get Set Go !
#
# 
#
# Create Term-Document Matrix for each Corpus
#
BJP_tdm = TermDocumentMatrix(BJP_tweet_corpus)
#
Cong_tdm = TermDocumentMatrix(Cong_tweet_corpus)
#
AAP_tdm = TermDocumentMatrix(AAP_tweet_corpus)
#
# convert as matrix
#
BJP.m = as.matrix(BJP_tdm)
Cong.m = as.matrix(Cong_tdm)
AAP.m = as.matrix(AAP_tdm)
#
#get word counts for each term-document matrix in decreasing order
#
BJP_word_freqs = sort(rowSums(BJP.m), decreasing=TRUE)
Cong_word_freqs = sort(rowSums(Cong.m), decreasing=TRUE) 
AAP_word_freqs = sort(rowSums(AAP.m), decreasing=TRUE) 
#
# create a data frame with words and their frequencies
BJP.dm = data.frame(word=names(BJP_word_freqs), freq=BJP_word_freqs)
Cong.dm = data.frame(word=names(Cong_word_freqs), freq=Cong_word_freqs)
AAP.dm = data.frame(word=names(AAP_word_freqs), freq=AAP_word_freqs)
#
#
# Finally Time to create the WordCloud #
#########################################
################ Modi####################
#########################################
wordcloud(BJP.dm$word, BJP.dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#
#
# 
#########################################
################ Rahul###################
#########################################
#
#
wordcloud(Cong.dm$word, Cong.dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#
#
#
#########################################
################### Arvind ##############
#########################################
#
#
wordcloud(AAP.dm$word, AAP.dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#
#
#
# This looks better !!!
#
# Now we know the general verbiage being used by/for various political leaders on Twitter
#
# Hope you guys have enjoyed this . Lot more to come !!!
#
# 
