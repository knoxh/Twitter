source("retweet.R")
status_id <- "948372959802085376"

# for calling specific text of tweet: look up status id and input
status_id <- "991074716348768256"
search <- search_tweets('"The tapes were just the beginning. May 18."', n=31000, include_rts = TRUE, retryonratelimit = TRUE)

directory <- make_status_directory(path=getwd(), status_id)
originalTweeter <- originalTweeter(status_id)
retweets_text <- rt(search)
non_retweets <- non_retweets(search, retweets_text)
weird_tweets <- weird_tweets(search, non_retweets)
who_retweet <- rtweetid(search, retweets_text)
# Should I continue the same way I would as before? Should I make files
#  for each of the new retweeters and proceed like before?

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

