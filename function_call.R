source("retweet.R")

# for calling specific text of tweet: look up status id and input
status_id <- "991074716348768256"
search <- search_tweets('"The tapes were just the beginning. May 18."', n=31000, include_rts = TRUE, retryonratelimit = TRUE)

directory <- make_status_directory(path=getwd(), status_id)
originalTweeter <- originalTweeter2(status_id)
retweets_text <- rt(search)
weird_tweets <- check(search)
who_retweet <- rtweetid(search, originalTweeter, directory)
retweeters <- getRetweeters(status_id, directory)
retweetersID <- getFollowers(who_retweet, status_id, directory)
edgeList <- createEdgeList(directory, retweetersID)
network <- graph(edgeList, originalTweeter)

####################################################################################
# calling functions 
directory <- make_status_directory(path=getwd(), status_id)
originalTweeter <- originalTweeter(status_id)
who_retweet <- initializeFiles(status_id, originalTweeter, directory)
retweeters <- getRetweeters(status_id, directory)
retweetersID <- getFollowers(who_retweet, status_id, directory)
edgeList <- createEdgeList(directory, retweetersID)
network <- graph(edgeList, originalTweeter)

# write csv for gephi
write.csv(edgeList, file=status_id)

# analysis networks
betweennessnetwork <- edgebetweenness(edgeList, originalTweeter)
closenessnetwork <- closeness(edgeList)
degreenetwork <- degreecentrality(edgeList)

