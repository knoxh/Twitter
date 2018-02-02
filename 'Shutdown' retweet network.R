source("twitter_authenticate.R")

tweets = searchTwitter("shutdown", n=1000)
id <- lapply(tweets, id)
retweet <- lapply(id, retweeters, n=20)
retweet
s <- strsplit(retweets, ": ")

# first element of each vector is the user name
getFirstElement <- function(x) {
  x[1]
}

getSecondElement <- function(x) {
  x[2]
}

retweeters <- lapply(s, getFirstElement)

s2 <- sapply(s, getSecondElement)
retweetedFrom <- gsub("RT@", "", s2)
#___________________________________________________________________
tweets = searchTwitter("shutdown", n=500)
tweets_txt = sapply(tweets, function(x) x$getText())
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets, ignore.case=TRUE, value=TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets_txt, ignore.case=TRUE)
head(tweets_txt[rt_patterns],10)
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

head(who_post,10)
head(who_retweet,10)

library(igraph)
library(stringr)

retweeter_poster = cbind(who_retweet, who_post)
rt_graph = graph.edgelist(retweeter_poster)
glay = layout.fruchterman.reingold(rt_graph)
par(mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label=NA,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color="gray",
     vertex.label.cex=0.75,
     edge.arrow.size=0.6,
     edge.arrow.width=0.5,
     edge.width=1,
     edge.col="navy")
# add title
title("\nTweets with 'Shutdown':  Who retweets whom", 
      cex.main=1, col.main="navy") 

edgeList <- get.edgelist(rt_graph)
write.csv(retweeter_poster, 'shutdown.csv')

#__________________________________________________________________________
#Trying different ways of getting list of users who retweeted a tweet
sapply(usersList, function(x) x$getFollowers)
follower.object <- lookupUsers(userList$getFollowers)
userList <- lookupUsers(who_retweet)




#follow <- lapply(userList, function(x) x$getFollowers)

index <- 1:length(userList)
index <- 300:327
edgeList <- NULL
for (i in index){
  
  cat("User number ", i, "\n")
  user <- userList[[i]]
  user_follower_ids <- user$getFollowers()
  userFollowers = sapply(user_follower_ids, screenName)

  common <- intersect(userList, userFollowers)
  cat("common followers: ")
  cat(common)
  cat('\n')
  
  if (length(common) >0) {
    e <- cbind(user$screenName, common)
    edgeList <- rbind(edgeList, e)
  # need to create edge list
  }

}

u <- "C"
common <- c("B","D")

cbind(u,common)


